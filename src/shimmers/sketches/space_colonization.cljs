(ns ^:figwheel-hooks shimmers.sketches.space-colonization
  "Inspired by https://thecodingtrain.com/CodingChallenges/017-spacecolonizer.html and
  https://medium.com/@jason.webb/space-colonization-algorithm-in-javascript-6f683b743dc5
  Algorithm is from http://algorithmicbotany.org/papers/colonization.egwnp2007.html"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.algorithm.space-colonization :as colonize]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]))

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

(defn setup []
  ;; (.clear js/console)
  (q/frame-rate 15)
  (colonize/create-tree [(q/width) (q/height)] @settings))

(defn update-state [state]
  (cq/if-steady-state
   state 5
   (fn [] (colonize/create-tree [(q/width) (q/height)] @settings))
   colonize/grow))

(defn draw-attractor [[x y] influence prune]
  (q/stroke-weight 0.2)
  (q/stroke "lightblue")
  (cq/circle x y influence)
  (q/stroke "red")
  (cq/circle x y prune))

(defn draw-debug
  [{:keys [attractors influence-distance prune-distance quadtree]} debug]

  (when (:attractors debug)
    (doseq [[x y] attractors]
      (q/stroke-weight 1)
      (q/stroke "green")
      (q/point x y)))

  (when ((some-fn :bubbles :influenced-by :next-branch) debug)
    (let [influencers (colonize/influencing-attractors attractors quadtree influence-distance)]
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
                  (v/add (:position branch)
                         (v/scale (colonize/average-attraction branch active-attractors) 5))))))))

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

(defn explanation []
  [:div
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
    (ctrl/checkbox settings "Show Next Branch Direction" [:debug :next-branch])]])

(sketch/defquil space-colonization
  :created-at "2020-11-27"
  :on-mount (fn [] (ctrl/mount explanation))
  :size [600 400]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])

;; Temporarily disable so it doesn't load on saving other sketches
;; ;; reload reagent components after figwheel save
;; (defn ^:after-load after-reload []
;;   (ctrl/mount explanation))
