(ns shimmers.sketches.butterfly
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]))

(defn wing [angle]
  (q/push-matrix)
  (q/rotate-y angle)
  (q/begin-shape)
  (q/curve-vertex 5 -10 0)
  (q/curve-vertex 3 -40 0)
  (q/curve-vertex 75 -70 0)
  (q/curve-vertex 100 -60 0)
  (q/curve-vertex 90 -20 0)
  (q/curve-vertex 70 0 0)
  (q/curve-vertex 25 -10 0)
  (q/curve-vertex 3  -10 0)
  (q/curve-vertex 3  -10 0)
  (q/end-shape :close)
  (q/begin-shape)
  (q/curve-vertex 5 40 0)
  (q/curve-vertex 5 -5 0)
  (q/curve-vertex 50 0 0)
  (q/curve-vertex 60 10 0)
  (q/curve-vertex 40 40 0)
  (q/curve-vertex 30 60 0)
  (q/curve-vertex 3 40 0)
  (q/curve-vertex 3 40 0)
  (q/end-shape :close)
  (q/pop-matrix))

(defn draw [_]
  (q/background 255)
  (q/stroke 0 160)

  (let [theta (/ (q/frame-count) 10)
        noise (* 2 (q/noise (/ theta 50)))]
    ;; add some pitch & yaw
    (q/rotate-z (q/sin (/ theta 20)))
    (q/rotate-x noise)
    ;; body
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
