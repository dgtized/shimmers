(ns shimmers.particles
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

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
  (let [[px py] position
        [vx vy] velocity
        [dx dy] acceleration
        new-position [(+ px vx) (+ py vy)]
        wrapped-position (wrap-around new-position)
        new-velocity [(+ vx dx) (+ vy dy)]]
    {:last-pos (if (= wrapped-position new-position) position wrapped-position)
     :position wrapped-position
     :velocity (constrain2d new-velocity -1 1)
     :acceleration (q/random-2d)}))

(defn setup []
  (q/background "white")
  {:particles (repeatedly 100 make-particle)})

(defn update-state [state]
  (update-in state [:particles] (partial map update-particle)))

(defn draw [{:keys [particles]}]
  ;; (q/background 255 10)
  (q/stroke 50 20)
  (doseq [{:keys [position last-pos]} particles]
    (let [[lx ly] last-pos
          [x y] position]
      (q/line lx ly x y))))

(defn ^:export run-sketch []
  (q/defsketch particles
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode]))


