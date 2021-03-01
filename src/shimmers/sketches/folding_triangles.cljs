(ns shimmers.sketches.folding-triangles
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]))

(defn setup []
  {})

(defn update-state [state]
  state)

(defn draw [state]
  (q/background 255)
  (q/rotate-y (/ (q/millis) 1000))
  (q/scale 50)
  (.normalMaterial (q/current-graphics))
  (q/begin-shape :triangles)
  (q/vertex 0 0 0)
  (q/vertex 0 1 0)
  (q/vertex 1 0 0)
  (q/end-shape))

(defn ^:export run-sketch []
  (q/defsketch folding-triangles
    :host "quil-host"
    :size [600 400]
    :renderer :p3d
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
