(ns shimmers.sketches.solar-transit
  "adapted from https://blog.exupero.org/mapping-elliptical-orbits/"
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [thi.ng.math.core :as tm]
   [thi.ng.geom.core :as g]))

(defrecord Body [name semi-major-axis eccentricity])

(defn planet [name semi-major-axis eccentricity]
  (->Body name semi-major-axis eccentricity))

(def orbits [(planet "Mercury" 5.791e10 0.20563)
             (planet "Venus" 1.082e11 0.006772)
             (planet "Earth" 1.496e11 0.0167086)
             (planet "Mars" 2.279e11 0.0934)
             (planet "Jupiter" 7.785e11 0.0489)
             (planet "Saturn" 1.434e12 0.0565)])

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

(defn orbital-path [body]
  (butlast (orbit-ellipse body)))

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
      (doseq [pos (orbital-path body)]
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
