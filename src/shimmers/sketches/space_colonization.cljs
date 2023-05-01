(ns ^:figwheel-hooks shimmers.sketches.space-colonization
  "Inspired by https://thecodingtrain.com/CodingChallenges/017-spacecolonizer.html and
  https://medium.com/@jason.webb/space-colonization-algorithm-in-javascript-6f683b743dc5
  Algorithm is from http://algorithmicbotany.org/papers/colonization.egwnp2007.html"
  (:require
   [clojure.edn :as edn]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.algorithm.space-colonization :as colonize]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.sequence :as cs]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce defo (debug/state))
(defonce settings
  (ctrl/state
   {:influence-distance 48
    :prune-distance 6
    :segment-distance 4
    :attractor-power 9
    :snap-theta 0
    :debug {:attractors true
            :bubbles false
            :canalization true
            :influenced-by false
            :next-branch false}}))

(defn attractor-gen
  [{[width height] :size} mode]
  (let [top 25
        bottom 30]
    (condp = mode
      :triangle
      (let [base (- height bottom)
            left (/ width 5)
            right (- width left)]
        (partial triangle/random-point-inside
                 (gt/triangle2
                  [left base]
                  [(/ width 2) 0]
                  [right base])))
      :circle
      (partial rp/inside-circle
               (gc/circle (cq/rel-vec 0.5 0.5)
                          (cq/rel-h (dr/random 0.2 0.45))))
      :square
      (let [left (/ width 6)]
        (partial g/random-point-inside
                 (rect/rect left top
                            (- width (* left 2))
                            (- height top bottom)))))))

(defn gen-root [{[w h] :size} roots]
  (mapv (fn [base]
          (colonize/make-root (gv/vec2 (* w base) (- h 10))))
        (cs/centered-range roots)))

(defn generate-tree
  [{:keys [attractor-power snap-theta] :as settings}]
  (let [bounds (cq/screen-rect)
        attractors
        (repeatedly (Math/pow 2 attractor-power)
                    (attractor-gen bounds (rand-nth [:triangle :square :circle])))
        roots (dr/weighted {1 4
                            2 2
                            3 1})]
    (-> settings
        (assoc :snap-theta (if (string? snap-theta) (edn/read-string snap-theta) snap-theta)
               :bounds bounds
               :branches (gen-root bounds roots)
               :attractors attractors)
        colonize/create-tree)))

(defn setup []
  (q/frame-rate 15)
  (generate-tree @settings))

(defn update-state [state]
  (cq/if-steady-state
   state 5
   (fn [] (generate-tree @settings))
   (fn [s]
     (let [{:keys [steady-state] :as s'}
           (colonize/grow s)]
       [steady-state s']))))

(defn draw-attractor [pos influence prune]
  (q/stroke-weight 0.2)
  (q/stroke "lightblue")
  (cq/circle pos influence)
  (q/stroke "red")
  (cq/circle pos prune))

(defn draw-debug
  [{:keys [attractors branches influence-distance prune-distance] :as state} debug]
  (reset! defo {:attractors (count attractors)
                :branches (count branches)})

  (when (:attractors debug)
    (doseq [[x y] attractors]
      (q/stroke-weight 1)
      (q/stroke "green")
      (q/point x y)))

  (when ((some-fn :bubbles :influenced-by :next-branch) debug)
    (let [influencers (colonize/influencing-attractors state)]
      (doseq [[branch active-attractors] influencers]
        (doseq [attractor active-attractors]
          (when (:bubbles debug)
            (draw-attractor attractor influence-distance prune-distance))

          (when (:influenced-by debug)
            (q/stroke-weight 0.05)
            (q/stroke 128 128)
            (q/line (:position branch) attractor)))

        (when (:next-branch debug)
          (q/stroke-weight 0.2)
          (q/stroke 0 0 200 128)
          (q/line (:position branch)
                  (tm/+ (:position branch)
                        (g/scale (colonize/average-attraction branch active-attractors 0 (gv/vec2)) 5))))))))

(defn draw [{:keys [branches weights] :as state}]
  (let [debug (:debug @settings)]
    (q/ellipse-mode :radius)
    (q/background "white")
    (q/no-fill)

    (q/stroke "black")

    (doseq [branch branches]
      (when-let [parent (:parent branch)]
        (q/stroke-weight (if (:canalization debug) (get weights branch) 0.2))
        (q/line (:position (nth branches parent)) (:position branch))))

    (draw-debug state debug)))

(defn ui-controls []
  [:div.flexcols
   (ctrl/container
    [:section
     [:b "Applies on next run:"]
     (ctrl/slider settings (fn [v] (str "Attractor Count " (Math/pow 2 v)))
                  [:attractor-power] [4 12])
     (ctrl/slider settings (fn [v] (str "Influence Distance " v))
                  [:influence-distance] [10 100])
     (ctrl/slider settings (fn [v] (str "Prune Distance " v))
                  [:prune-distance] [2 50])
     (ctrl/slider settings (fn [v] (str "Segment Distance " v))
                  [:segment-distance] [1 30])
     (ctrl/dropdown settings
                    "Snap Angles To " [:snap-theta]
                    {"Disabled" 0
                     "90 degrees" (/ Math/PI 2)
                     "60 degrees" (/ Math/PI 3)
                     "45 degrees" (/ Math/PI 4)
                     "30 degrees" (/ Math/PI 6)})]
    [:br]
    [:section
     [:b "Applies immediately:"]
     (ctrl/checkbox settings "Show Canalization" [:debug :canalization])
     (ctrl/checkbox settings "Show Attractors" [:debug :attractors])
     (ctrl/checkbox settings "Show Influence/Prune Bubbles" [:debug :bubbles])
     (ctrl/checkbox settings "Show Influence-By Lines" [:debug :influenced-by])
     (ctrl/checkbox settings "Show Next Branch Direction" [:debug :next-branch])])
   (debug/display defo)])

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:div.contained.explanation
    [ui-controls]]])

(sketch/definition space-colonization
  {:created-at "2020-11-27"
   :tags #{}
   :type :quil}
  (ctrl/mount page "sketch-host"))

;; Temporarily disable so it doesn't load on saving other sketches
;; ;; reload reagent components after figwheel save
;; (defn ^:after-load after-reload []
;;   (ctrl/mount ui-controls))
