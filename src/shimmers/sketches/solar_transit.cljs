(ns shimmers.sketches.solar-transit
  "adapted from https://blog.exupero.org/mapping-elliptical-orbits/"
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.matrix :as mat]
   [thi.ng.math.core :as tm]))

(defrecord Body [name
                 semi-major-axis
                 inclination
                 longitude-of-ascending-node
                 argument-of-perihelion
                 eccentricity
                 period
                 mean-anomaly-at-epoch])

(defn planet
  "Convert parameters from degrees to radians"
  [name
   semi-major-axis
   inclination
   longitude-of-ascending-node
   argument-of-perihelion
   eccentricity
   period
   mean-anomaly-at-epoch]
  (->Body name
          semi-major-axis
          (tm/radians inclination)
          (tm/radians longitude-of-ascending-node)
          (tm/radians argument-of-perihelion)
          eccentricity
          period
          (tm/radians mean-anomaly-at-epoch)))

(def orbits
  [(planet "Mercury" 5.791e10 7.005 48.331 29.124 0.20563 87.97 174.796)
   (planet "Venus" 1.082e11 3.39458 76.86 54.884 0.006772 224.70 50.115)
   (planet "Earth" 1.496e11 5.0E-5 -11.26064 114.20783 0.0167086 365.26 358.617)
   (planet "Mars" 2.279e11 1.85 49.558 286.502 0.0934 686.98 19.412)
   (planet "Jupiter" 7.785e11 1.303 100.464 273.867 0.0489 4332.59 20.02)
   (planet "Saturn" 1.434e12 2.485 113.665 339.392 0.0565 10759.22 317.02)
   (planet "Uranus" 2.871e12 0.773 74.006 96.998857 0.04717 30688.50 142.2386)
   (planet "Neptune" 4.500e12 1.77 131.783 273.187 0.008678 60195.00 256.228)
   (planet "Pluto" 5.906e12 17.16 110.299 113.834 0.2488 90560.00 14.53)])

(defn radial-dist [semi-major-axis eccentricity]
  (fn [theta]
    (/ (* semi-major-axis (- 1 (eq/sqr eccentricity)))
       (inc (* eccentricity (Math/cos theta))))))

(defn orbit-ellipse [{:keys [semi-major-axis eccentricity]}]
  (let [radius (radial-dist semi-major-axis eccentricity)]
    (map (fn [t]
           (let [theta (* eq/TAU t)]
             (v/polar (radius theta) theta)))
         (tm/norm-range 60))))

(defn orbit [{:keys [argument-of-perihelion
                     inclination
                     longitude-of-ascending-node]
              :as body}]
  (sequence
   (comp
    (map (fn [pos] (g/transform pos (g/rotate-z mat/M44 argument-of-perihelion))))
    (map (fn [pos] (g/transform pos (g/rotate-x mat/M44 inclination))))
    (map (fn [pos] (g/transform pos (g/rotate-z mat/M44 longitude-of-ascending-node)))))
   (butlast (orbit-ellipse body))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn draw [_]
  (q/background 1.0)
  (q/no-fill)
  (q/stroke 0.0)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (q/scale 1 -1)
  (let [max-orbit (apply max (map :semi-major-axis orbits))
        scale (/ (* 2.3 max-orbit) (max (q/width) (q/height)))]
    (doseq [body orbits]
      (q/begin-shape)
      (doseq [pos (orbit body)]
        (apply q/curve-vertex (g/scale pos (/ 1 scale))))
      (q/end-shape :close))))

(defn page []
  [:div
   (sketch/component
    :size [600 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition solar-transit
  {:created-at "2023-04-26"
   :type :quil
   :tags #{}}
  (ctrl/mount page "sketch-host"))
