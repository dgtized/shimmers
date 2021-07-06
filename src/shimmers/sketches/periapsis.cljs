(ns shimmers.sketches.periapsis
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.probability :as p]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.math.core :as tm]))

(defrecord Body [mass radius dtheta theta0])

(defn make-bodies [n]
  (cons
   (Body. 64 0 0 0)
   (for [i (range n)]
     (Body. (tm/random 8 16)
            (+ (* 0.5 (q/random-gaussian))
               (tm/map-interval i 0 n 48 (/ (q/width) 2)))
            (if (p/chance 0.1)
              (tm/random -0.05)
              (tm/random 0.1))
            (tm/random tm/TWO_PI)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:bodies (make-bodies 200)
   :t 0.0})

(defn update-state [state]
  (update state :t + 0.05))

(defn draw [{:keys [bodies t]}]
  (q/ellipse-mode :radius)
  (q/background 1.0 0.25)
  (q/no-stroke)
  (q/fill 0.0 0.7)
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
