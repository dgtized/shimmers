(ns shimmers.sketches.folding-triangles
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.triangle :as gt]))

(defn setup []
  {})

(defn update-state [state]
  state)

(defn draw [_]
  (let [triangle (gt/triangle3 [0 -5 0] [0 10 0] [10 0 0])]
    (q/background 255)
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
