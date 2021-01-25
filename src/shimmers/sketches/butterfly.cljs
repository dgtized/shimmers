(ns shimmers.sketches.butterfly
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]))

(defn wing []
  (q/begin-shape)
  (q/vertex 5 -10 0)
  (q/vertex 5 -40 0)
  (q/vertex 75 -70)
  (q/vertex 100 -60 0)
  (q/vertex 90 -20 0)
  (q/vertex 70 0 0)
  (q/end-shape :close)
  (q/begin-shape)
  (q/vertex 5 40 0)
  (q/vertex 5 -5 0)
  (q/vertex 50 0 0)
  (q/vertex 40 40 0)
  (q/end-shape :close))

(defn draw [_]
  (q/background 255)

  (q/ellipsoid 5 50 5)
  (wing)
  (q/rotate-y Math/PI)
  (wing)
  )

(defn ^:export run-sketch []
  (q/defsketch butterfly
    :host "quil-host"
    :renderer :p3d
    :size [600 400]
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
