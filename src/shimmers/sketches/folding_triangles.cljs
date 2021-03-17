(ns shimmers.sketches.folding-triangles
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]
            [thi.ng.geom.quaternion :as quat]
            [thi.ng.geom.vector :as tv]))

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

(defn draw [{:keys [theta]}]
  (q/background 255)
  (q/push-matrix)
  (let [a (gv/vec3 [0 -5 0])
        b (gv/vec3 [5 10 0])
        c (gv/vec3 [10 0 0])
        triangle (gt/triangle3 a b c)]
    (apply q/translate (geom/centroid triangle))
    (q/scale 5)
    (cq/draw-shape (geom/vertices triangle))
    (doseq [edge (geom/edges triangle)]
      (cq/draw-shape (geom/vertices (rotate-over-edge triangle edge theta)))))
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
