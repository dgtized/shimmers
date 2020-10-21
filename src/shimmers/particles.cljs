(ns shimmers.particles
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.vector :as v]))

(defn make-particle []
  (let [initial-pos [(q/random (q/width)) (q/random (q/height))]]
    {:last-pos initial-pos
     :position initial-pos
     :velocity (q/random-2d)
     :acceleration (q/random-2d)}))

(defn wrap-value [x lower upper]
  (if (< x lower) upper
      (if (> x upper) lower
          x)))

(defn wrap-around [[x y]]
  [(wrap-value x 0 (q/width))
   (wrap-value y 0 (q/height))])

(defn constrain2d [[x y] lower upper]
  [(q/constrain x lower upper)
   (q/constrain y lower upper)])

(defn update-particle
  [{:keys [position velocity acceleration]} particle]
  (let [new-velocity (-> (v/add velocity acceleration) (constrain2d -1.5 1.5))
        new-position (v/add position new-velocity)
        wrapped-position (wrap-around new-position)]
    {:last-pos (if (= wrapped-position new-position) position wrapped-position)
     :position wrapped-position
     :velocity new-velocity
     :acceleration (v/scale (q/random-2d) 0.5)}))

(defn setup []
  (q/background "white")
  {:particles (repeatedly 100 make-particle)})

(defn update-state [state]
  (update-in state [:particles] (partial map update-particle)))

(defn draw [{:keys [particles]}]
  ;; (q/background 256 16)
  (q/stroke 128 32)
  (doseq [{:keys [position last-pos]} particles]
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


