(ns shimmers.sketches.periapsis
  (:require [kixi.stats.distribution :as ksd]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.probability :as p]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.math.core :as tm]))

;; Math cobbled together from:
;; https://en.wikipedia.org/wiki/Ellipse
;; https://en.wikipedia.org/wiki/Orbit_equation
;; https://en.wikipedia.org/wiki/Elliptic_orbit
(defn semi-minor [eccentricity semi-major]
  (* semi-major (Math/sqrt (- 1.0 (Math/pow eccentricity 2)))))

;; TODO: Consider making speed proportional to mass and semi-major to match
;; escape velocities correctly?
(defrecord Body [mass semi-major semi-minor focal-distance rotation speed theta0 moons])

;; TODO: Fix offsets somehow so children cannot clip parents they orbit?
(defn position [{:keys [semi-major semi-minor focal-distance
                        speed theta0]} t]
  (let [theta (+ theta0 (* speed t))]
    (v/vec2 (+ focal-distance (* semi-major (Math/cos theta)))
            (* semi-minor (Math/sin theta)))))

(defn make-body [{:keys [semi-major eccentricity] :as params
                  :or {semi-major 0
                       eccentricity 0}}]
  (map->Body (merge {:semi-major semi-major
                     :semi-minor (semi-minor eccentricity semi-major)
                     :focal-distance (* eccentricity semi-major)
                     :speed 0
                     :theta0 (tm/random tm/TWO_PI)
                     :rotation (tm/random tm/TWO_PI)
                     :moons []}
                    params)))

(defn make-moon []
  (make-body {:semi-major (tm/random 8 24)
              :eccentricity (p/happensity 0.3)
              :mass (/ (tm/random 1 4) 4)
              :speed (if (p/chance 0.1)
                       (tm/random -0.4)
                       (tm/random 0.8))}))

(defn make-bodies [n]
  (cons
   (make-body {:mass 16})
   (for [i (range n)]
     (make-body
      {:semi-major (+ (* 0.5 (q/random-gaussian))
                      (tm/map-interval i 0 n 52 (/ (q/width) 2)))
       ;; eccentricity likelyhood is proportional to to size of orbit for aesthetics
       :eccentricity (p/happensity (* 0.75 (tm/smoothstep* (/ n 16) (/ n 1.3) i)))
       :mass (/ (tm/random 8 12) 4)
       :speed (if (p/chance 0.1)
                (tm/random -0.05)
                (tm/random 0.1))
       :moons (repeatedly (ksd/draw (ksd/poisson {:lambda 0.9})) make-moon)}))))

(comment
  (->> (ksd/poisson {:lambda 0.9})
       (ksd/sample 192)
       frequencies))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:bodies (make-bodies 192)
   :t 0.0})

(defn update-state [state]
  (update state :t + 0.05))

(defn draw [{:keys [bodies t]}]
  (q/ellipse-mode :radius)
  (q/background 1.0 0.25)
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
