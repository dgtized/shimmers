(ns shimmers.sketches.folding-triangles
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.quaternion :as quat]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

;; Quaternion
;; https://www.weizmann.ac.il/sci-tea/benari/sites/sci-tea.benari/files/uploads/softwareAndLearningMaterials/quaternion-tutorial-2-0-1.pdf
;; http://danceswithcode.net/engineeringnotes/quaternions/quaternions.html
;; https://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation
;; https://en.wikipedia.org/wiki/Gimbal_lock
;; https://developerblog.myo.com/quaternions/

(defn setup []
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn rotate-over-edge [poly [a b] theta]
  (let [axis (tm/- b a)
        rotation (quat/quat-from-axis-angle axis theta)]
    (-> poly
        (geom/translate (tm/- a))
        (geom/transform rotation)
        (geom/translate a))))

(defn reflect-over-edge [c [a b]]
  (let [edge (gl/line3 a b)
        close (gl/line3 c (geom/closest-point edge c))]
    (first (:points (geom/reflect close edge)))))

(defn unfurled [triangle]
  (let [[a b c] (:points triangle)]
    (gt/triangle3 (reflect-over-edge c [a b])
                  (reflect-over-edge b [a c])
                  (reflect-over-edge a [b c]))))

(defn draw [_]
  (q/background 255)
  (let [depth 10
        theta (mod (/ (q/millis) 500) (* depth 6 Math/PI))
        base (-> (gt/equilateral2 1 1.5)
                 (geom/center (gv/vec3))
                 (geom/rotate (/ theta 12)))
        all
        (mapcat (fn [triangle i]
                  (let [start (* Math/PI i)
                        end (+ start (* (- depth i) 6 Math/PI))]
                    (if (and (> theta start) (< theta end))
                      (map (fn [edge]
                             (assoc (rotate-over-edge triangle edge (- theta start))
                                    :color (mod (- (* 0.1 i) 0.5) 1.0)))
                           (geom/edges triangle))
                      [])))
                (take depth (iterate unfurled base))
                (take depth (iterate inc 0)))]
    (q/scale 3)
    (doseq [t all]
      (q/fill (:color t) 0.8 0.5 0.1)
      (cq/draw-shape (geom/vertices t)))))

(defn ^:export run-sketch []
  (q/defsketch folding-triangles
    :host "quil-host"
    :size [600 400]
    :renderer :p3d
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
