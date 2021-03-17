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

(defn draw [{:keys [theta]}]
  (q/background 255)
  (q/push-matrix)
  (q/translate -100 -100)
  (let [a (gv/vec3 [-80 -50 0])
        b (gv/vec3 [-40 -55 0])
        c (gv/vec3 [-60 -30 0])
        close (gl/line3 c (geom/closest-point (gl/line3 a b) c))
        reflect (geom/reflect close (gl/line3 a b))
        reflect-c (first (:points reflect))]
    (doseq [s [(gt/triangle3 a b c)
               close
               reflect
               (gt/triangle3 a b reflect-c)]]
      (cq/draw-shape (geom/vertices s))))
  (q/push-matrix)
  (q/translate 150 -50)
  (q/scale 2)
  (let [a (gv/vec3 [0 0 0])
        b (gv/vec3 [2 4 0])
        c (gv/vec3 [4 1 0])]
    (doseq [s [(geom/rotate-around-axis (gt/triangle3 a b c) b
                                        theta)]]
      (cq/draw-shape (geom/vertices s))))
  (q/pop-matrix)
  (q/push-matrix)
  (let [a (gv/vec3 [0 0 0])
        b (gv/vec3 [5 10 0])
        c (gv/vec3 [10 0 0])
        triangle (gt/triangle3 a b c)
        axis (tm/- b c)
        r (quat/quat-from-axis-angle axis theta)
        offset (geom/translate (gt/triangle3 a b c) (gv/vec3 -10 0 0))
        rotated (geom/translate (geom/transform offset r) (gv/vec3 10 0 0))
        ]
    (q/translate 150 170)
    (q/scale 5)
    (q/line (gv/vec3) axis)
    (cq/draw-shape (geom/vertices triangle))
    (cq/draw-shape (geom/vertices offset))
    (cq/draw-shape (geom/vertices rotated)))
  (q/pop-matrix)
  (let [triangle (gt/triangle3 [0 -5 0] [0 10 0] [10 0 0])]
    (q/push-matrix)
    (q/rotate-y theta)
    (q/scale 10)
    (cq/draw-shape (geom/vertices triangle))
    (q/pop-matrix)))

(defn ^:export run-sketch []
  (q/defsketch folding-triangles
    :host "quil-host"
    :size [600 400]
    :renderer :p3d
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
