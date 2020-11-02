(ns shimmers.particles-random-walk
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.math.vector :as v]
            [shimmers.math.color :as color]))

(defn make-particle []
  (let [initial-pos (v/vec2 (q/random (q/width)) (q/random (q/height)))]
    {:last-pos initial-pos
     :position initial-pos
     :velocity (v/vec2 (q/random-2d))
     :acceleration (v/vec2 (q/random-2d))
     :color (color/random)}))

(defn update-particle
  [{:keys [position velocity acceleration] :as particle}]
  (let [new-velocity (-> (v/add velocity acceleration) (v/constrain2d -1.5 1.5))
        new-position (v/add position new-velocity)
        wrapped-position (v/wrap2d new-position (q/width) (q/height))]
    (assoc particle
           :last-pos (if (= wrapped-position new-position) position wrapped-position)
           :position wrapped-position
           :velocity new-velocity
           :acceleration (v/scale (v/vec2 (q/random-2d)) 0.5))))

(defn setup []
  (q/background "white")
  {:particles (repeatedly 50 make-particle)})

(defn update-state [state]
  (update-in state [:particles] (partial map update-particle)))

(defn draw [{:keys [particles]}]
  ;; (q/background 256 16)
  (doseq [{:keys [position last-pos color]} particles]
    (apply q/stroke color)
    (let [[lx ly] last-pos
          [x y] position]
      (q/line lx ly x y))))

(defn ^:export run-sketch []
  (q/defsketch particles
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode]))


