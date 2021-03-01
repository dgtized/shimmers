(ns shimmers.sketches.folding-triangles
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as quil]))

(defn setup []
  {:texture (q/create-graphics 1 1)})

(defn shape-texture [t]
  (q/with-graphics t
    (q/no-stroke)
    (q/fill 200)
    (q/rect 0 0 0.1 1)
    (q/fill 100)
    (q/rect 0.5 0 0.5 1))
  t)

(defn update-state [state]
  state)

(defn draw [{:keys [texture]}]
  (q/background 255)
  (q/rotate-y (/ (q/millis) 1000))
  (q/scale 10)
  (q/texture (shape-texture texture))
  (.textureMode (q/current-graphics) "normal")
  (q/begin-shape)
  (q/vertex 0 0 0)
  (q/vertex 0 10 0)
  (q/vertex 10 0 0)
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
