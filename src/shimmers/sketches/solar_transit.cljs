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
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defrecord Body [name
                 semi-major-axis
                 argument-of-perihelion
                 eccentricity])

(defn planet [name semi-major-axis argument-of-perihelion eccentricity]
  (->Body name semi-major-axis argument-of-perihelion eccentricity))

(def orbits [(planet "Mercury" 5.791e10 29.124 0.20563)
             (planet "Venus" 1.082e11 54.884 0.006772)
             (planet "Earth" 1.496e11 114.20783 0.0167086)
             (planet "Mars" 2.279e11 286.502 0.0934)
             (planet "Jupiter" 7.785e11 273.867 0.0489)
             (planet "Saturn" 1.434e12 339.392 0.0565)])

(defn radial-dist [semi-major-axis eccentricity]
  (fn [theta]
    (/ (* semi-major-axis (- 1 (eq/sqr eccentricity)))
       (inc (* eccentricity (Math/cos theta))))))

(defn orbit-ellipse [{:keys [semi-major-axis eccentricity]}]
  (let [radius (radial-dist semi-major-axis eccentricity)]
    (map (fn [t]
           (let [theta (* eq/TAU t)]
             (gv/vec3 (v/polar (radius theta) theta))))
         (tm/norm-range 60))))

(defn orbit [{:keys [argument-of-perihelion] :as body}]
  (sequence
   (map (fn [pos] (g/transform pos (g/rotate-z mat/M44 (tm/radians argument-of-perihelion)))))
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
  (let [max-orbit (apply max (map :semi-major-axis orbits))
        scale (/ (* 2.2 max-orbit) (max (q/width) (q/height)))]
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
