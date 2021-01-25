(ns shimmers.sketches.butterfly
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]))

(defn wing [angle]
  (q/push-matrix)
  (q/rotate-y angle)
  (q/begin-shape)
  (q/vertex 5 -10 0)
  (q/vertex 5 -40 0)
  (q/vertex 75 -70 0)
  (q/vertex 100 -60 0)
  (q/vertex 90 -20 0)
  (q/vertex 70 0 0)
  (q/end-shape :close)
  (q/begin-shape)
  (q/vertex 5 40 0)
  (q/vertex 5 -5 0)
  (q/vertex 50 0 0)
  (q/vertex 60 10 0)
  (q/vertex 40 40 0)
  (q/end-shape :close)
  (q/pop-matrix))

(defn draw [_]
  (q/background 255)

  (let [theta (/ (q/frame-count) 10)]
    (q/ellipsoid 5 50 5)
    (wing (* 0.9 (q/cos theta)))
    (wing (- Math/PI (* 0.9 (q/cos theta))))))

(defn ^:export run-sketch []
  (q/defsketch butterfly
    :host "quil-host"
    :renderer :p3d
    :size [600 400]
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
