(ns shimmers.sketches.periapsis
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.probability :as p]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.math.core :as tm]))

(defrecord Body [mass radius dtheta theta0 moons])
(defrecord Body [mass semi-major dtheta theta0 moons])

(defn make-moon []
  (map->Body
   {:mass (/ (tm/random 1 4) 4)
    :semi-major (tm/random 8 24)
    :dtheta (if (p/chance 0.1)
              (tm/random -0.4)
              (tm/random 0.8))
    :theta0 (tm/random tm/TWO_PI)
    :moons []}))

(defn make-bodies [n]
  (cons
   (Body. 16 0 0 0 [])
   (for [i (range n)]
     (map->Body
      {:mass (/ (tm/random 8 12) 4)
       :semi-major (+ (* 0.5 (q/random-gaussian))
                      (tm/map-interval i 0 n 48 (/ (q/width) 2)))
       :dtheta (if (p/chance 0.1)
                 (tm/random -0.05)
                 (tm/random 0.1))
       :theta0 (tm/random tm/TWO_PI)
       :moons (if (p/chance 0.3)
                (repeatedly (rand-int 4) make-moon)
                [])}))))

(defn position [{:keys [semi-major dtheta theta0]} t]
  (v/polar semi-major (+ theta0 (* dtheta t))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:bodies (make-bodies 256)
   :t 0.0})

(defn update-state [state]
  (update state :t + 0.05))

(defn draw [{:keys [bodies t]}]
  (q/ellipse-mode :radius)
  (q/background 1.0 0.2)
  (q/no-stroke)
  (q/fill 0.0 0.7)
  (q/with-translation (cq/rel-pos 0.5 0.5)
    (doseq [{:keys [mass moons] :as body} bodies
            :let [pos (position body t)]]
      (cq/circle pos mass)
      (q/with-translation pos
        (doseq [{:keys [mass] :as moon} moons]
          (cq/circle (position moon t) mass))))))

(sketch/defquil periapsis
  :created-at "2021-07-06"
  :size [800 800]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
