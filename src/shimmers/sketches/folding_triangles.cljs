(ns shimmers.sketches.folding-triangles
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn setup []
  {})

(defn update-state [state]
  state)

(defn draw [_]
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
  (let [a (gv/vec3 [0 0 0])
        b (gv/vec3 [2 4 0])
        c (gv/vec3 [4 1 0])]
    (doseq [s [(geom/rotate-around-axis (gt/triangle3 a b c) (tm/- b a)
                                        (/ (q/millis) 1000))]]
      (cq/draw-shape (geom/vertices s))))
  (q/pop-matrix)
  (q/pop-matrix)
  (let [triangle (gt/triangle3 [0 -5 0] [0 10 0] [10 0 0])]
    (q/push-matrix)
    (q/rotate-y (/ (q/millis) 1000))
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
