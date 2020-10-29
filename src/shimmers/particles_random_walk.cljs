(ns shimmers.particles-random-walk
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.vector :as v]))

(defn wrap-around [[x y]]
  (v/vec2 (v/wrap-value x 0 (q/width))
          (v/wrap-value y 0 (q/height))))

(def colors [[128 32]
             [128 0 0 32]
             [0 0 128 32]
             [0 128 0 32]])

(defn make-particle []
  (let [initial-pos (v/vec2 (q/random (q/width)) (q/random (q/height)))]
    {:last-pos initial-pos
     :position initial-pos
     :velocity (v/vec2 (q/random-2d))
     :acceleration (v/vec2 (q/random-2d))
     :color (rand-nth colors)}))

(defn update-particle
  [{:keys [position velocity acceleration] :as particle}]
  (let [new-velocity (-> (v/add velocity acceleration) (v/constrain2d -1.5 1.5))
        new-position (v/add position new-velocity)
        wrapped-position (wrap-around new-position)]
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
    :size [400 300]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode]))


