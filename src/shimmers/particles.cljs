(ns shimmers.particles
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn make-particle []
  {:position [(q/random (q/width)) (q/random (q/height))]
   :velocity (q/random-2d)
   :acceleration (q/random-2d)})

(defn wrap-value [x lower upper]
  (if (< x lower) (- upper 1)
      (if (>= x upper) lower
          x)))

(defn wrap-around [[x y]]
  [(wrap-value x 0 (q/width))
   (wrap-value y 0 (q/height))])

(defn constrain2d [[x y] lower upper]
  [(q/constrain x lower upper)
   (q/constrain y lower upper)])

(defn update-particle
  [{:keys [position velocity acceleration]} particle]
  (let [[px py] position
        [vx vy] velocity
        [dx dy] acceleration]
    {:position (wrap-around [(+ px vx) (+ py vy)])
     :velocity (constrain2d [(+ vx dx) (+ vy dy)] -5 5)
     :acceleration (q/random-2d)}))

(defn setup []
  (q/background "white")
  {:particles (repeatedly 100 make-particle)})

(defn update-state [state]
  (update-in state [:particles] (partial map update-particle)))

(defn draw [{:keys [particles]}]
  (q/stroke 100 200)
  (doseq [{:keys [position]} particles]
    (let [[x y] position]
      (q/point x y))))

(defn ^:export run-sketch []
  (q/defsketch particles
    :host "quil-host"
    :size [500 500]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode]))


