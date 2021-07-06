(ns shimmers.sketches.periapsis
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]
            [shimmers.common.quil :as cq]
            [shimmers.math.vector :as v]))

(defrecord Body [mass radius dtheta theta0])

(defn setup []
  (q/color-mode :hsl 1.0)
  {:bodies [(Body. 100 0 0 0)
            (Body. 10 30 0.1 0)
            (Body. 20 50 -0.2 5)]
   :t 0.0})

(defn update-state [state]
  (update state :t + 0.1))

(defn draw [{:keys [bodies t]}]
  (q/background 1.0 0.25)
  (q/fill 0.0 0.5)
  (q/with-translation (cq/rel-pos 0.5 0.5)
    (doseq [{:keys [mass radius dtheta theta0]} bodies]
      (cq/circle (v/polar radius (+ theta0 (* dtheta t))) (/ mass 4)))))

(sketch/defquil periapsis
  :created-at "2021-07-06"
  :size [800 800]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
