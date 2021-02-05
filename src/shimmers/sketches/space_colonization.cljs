(ns ^:figwheel-hooks shimmers.sketches.space-colonization
  "Inspired by https://thecodingtrain.com/CodingChallenges/017-spacecolonizer.html and
  https://medium.com/@jason.webb/space-colonization-algorithm-in-javascript-6f683b743dc5
  Algorithm is from http://algorithmicbotany.org/papers/colonization.egwnp2007.html"
  (:require [goog.dom :as dom]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as quil]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.vector :as v]
            [shimmers.algorithm.space-colonization :as colonize]))

(defn init-settings []
  {:influence-distance 48
   :prune-distance 6
   :segment-distance 4
   :attractor-power 9
   :snap-theta 0
   :debug {:attractors true
           :bubbles false
           :canalization true
           :influenced-by false
           :next-branch false}})

(defonce settings (r/atom (init-settings)))
(defn setup []
  ;; (.clear js/console)
  (q/frame-rate 15)
  (colonize/create-tree [(q/width) (q/height)] @settings))

(defn update-state [state]
  (quil/if-steady-state
   state 5
   (fn [] (colonize/create-tree [(q/width) (q/height)] @settings))
   colonize/grow))

(defn draw-attractor [[x y] influence prune]
  (q/stroke-weight 0.2)
  (q/stroke "lightblue")
  (q/ellipse x y influence influence)
  (q/stroke "red")
  (q/ellipse x y prune prune))

(defn draw-debug [{:keys [attractors influence-distance prune-distance
                          quadtree] :as state} debug]
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

(defn mount-reagent
  "Mounts reagent component to render in explanation element.

  Helper method so it can be invoked on run-sketch OR on figwheel reload."
  []
  (rdom/render [explanation] (dom/getElement "explanation")))

(defn ^:export run-sketch []
  (mount-reagent)
  (q/defsketch space-colonization
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

;; Temporarily disable so it doesn't load on saving other sketches
;; ;; reload reagent components after figwheel save
;; (defn ^:after-load after-reload []
;;   (mount-reagent))
