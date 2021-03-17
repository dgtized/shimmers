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

(defn setup []
  {})

(defn update-state [state]
  (assoc state :theta (/ (q/millis) 1000)))

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

(comment (unfurled (gt/equilateral2 5 10)))

(defn draw [{:keys [theta]}]
  (q/background 255)
  (q/push-matrix)
  (let [triangle (geom/center (gt/equilateral2 5 10) (gv/vec3))]
    (q/scale 10)
    (doseq [edge (geom/edges triangle)]
      (cq/draw-shape (geom/vertices (rotate-over-edge triangle edge (+ Math/PI (mod (- theta) Math/PI))))))
    (let [u (unfurled triangle)]
      (doseq [edge (geom/edges u)]
        (cq/draw-shape (geom/vertices (rotate-over-edge u edge (+ Math/PI (mod (- theta) Math/PI))))))))
  (q/pop-matrix))

(defn ^:export run-sketch []
  (q/defsketch folding-triangles
    :host "quil-host"
    :size [600 400]
    :renderer :p3d
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
