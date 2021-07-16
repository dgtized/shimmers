(ns shimmers.sketches.periapsis
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.probability :as p]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.math.core :as tm]))

(defn semi-minor [eccentricity semi-major]
  (* semi-major (Math/sqrt (- 1.0 (Math/pow eccentricity 2)))))

(defrecord Body [mass semi-major semi-minor focal-distance rotation dtheta theta0 moons])

(defn make-body [{:keys [semi-major eccentricity] :as params
                  :or {semi-major 0
                       eccentricity 0}}]
  (map->Body (merge {:semi-major semi-major
                     :semi-minor (semi-minor eccentricity semi-major)
                     :focal-distance (* eccentricity semi-major)
                     :dtheta 0
                     :theta0 (tm/random tm/TWO_PI)
                     :rotation (tm/random tm/TWO_PI)
                     :moons []}
                    params)))

(defn make-moon []
  (make-body {:semi-major (tm/random 8 24)
              :eccentricity (* 1.5 (p/happensity 0.3))
              :mass (/ (tm/random 1 4) 4)
              :dtheta (if (p/chance 0.1)
                        (tm/random -0.4)
                        (tm/random 0.8))}))

(defn make-bodies [n]
  (cons
   (make-body {:mass 16})
   (for [i (range n)]
     (make-body
      {:semi-major (+ (* 0.5 (q/random-gaussian))
                      (tm/map-interval i 0 n 48 (/ (q/width) 2)))
       ;; eccentricty likelyhood is proportional to to size of orbit for aesthetics
       :eccentricity (* 0.8 (p/happensity (* 0.8 (tm/smoothstep* (/ n 16) (/ n 4) i))))
       :mass (/ (tm/random 8 12) 4)
       :dtheta (if (p/chance 0.1)
                 (tm/random -0.05)
                 (tm/random 0.1))
       :moons (if (p/chance 0.3)
                (repeatedly (rand-int 4) make-moon)
                [])}))))

(defn position [{:keys [semi-major semi-minor focal-distance
                        dtheta theta0]} t]
  (let [theta (+ theta0 (* dtheta t))]
    (v/vec2 (+ focal-distance (* semi-major (Math/cos theta)))
            (* semi-minor (Math/sin theta)))))

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
    (doseq [{:keys [mass moons rotation] :as body} bodies
            :let [pos (position body t)]]
      (q/with-rotation [rotation]
        (cq/circle pos mass)
        (q/with-translation pos
          (doseq [{:keys [mass rotation] :as moon} moons]
            (q/with-rotation [rotation]
              (cq/circle (position moon t) mass))))))))

(sketch/defquil periapsis
  :created-at "2021-07-06"
  :size [800 800]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
