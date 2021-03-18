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
  (assoc state :theta (/ (q/millis) 500)))

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

(defn draw [{:keys [theta]}]
  (q/background 255)
  (let [depth 7
        triangle (-> (gt/equilateral2 1 2)
                     (geom/center (gv/vec3))
                     (geom/rotate (/ theta 12)))
        all (mapcat (fn [t start]
                      (if (> theta start)
                        (map (fn [e]
                               (rotate-over-edge t e (- theta start)))
                             (geom/edges t))
                        []))
                    (take depth (iterate unfurled triangle))
                    (take depth (iterate (partial + Math/PI) 0)))]
    (q/scale 4)
    (q/fill 0.35 0.8 0.6 0.1)
    (doseq [t all]
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
