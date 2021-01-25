(ns shimmers.sketches.butterfly
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]))

;; TODO: Perturb parameters a little to generate random wings? Similarly is it
;; possible to flex the leading/trailing edges a little as a function of angle
;; to add some curl
;; TODO: Texture the wings?
;; TODO: Add some gliding or irregular flapping?
;; TODO: Instancing for multiple butterflies?
;; TODO: Boids?
(defn wing [angle]
  (q/push-matrix)
  (q/rotate-y angle)
  (q/begin-shape)
  (q/curve-vertex 0 -10 0)
  (q/curve-vertex 0 -40 0)
  (q/curve-vertex 75 -70 0)
  (q/curve-vertex 100 -60 0)
  (q/curve-vertex 90 -20 0)
  (q/curve-vertex 70 0 0)
  (q/curve-vertex 25 -10 0)
  (q/curve-vertex 0  -10 0)
  (q/curve-vertex 0  -10 0)
  (q/curve-vertex 0 40 0)
  (q/curve-vertex 0 -5 0)
  (q/curve-vertex 50 0 0)
  (q/curve-vertex 60 10 0)
  (q/curve-vertex 40 40 0)
  (q/curve-vertex 30 60 0)
  (q/curve-vertex 0 40 0)
  (q/curve-vertex 0 40 0)
  (q/end-shape :close)
  (q/pop-matrix))

(defn draw [_]
  (q/background 255)
  (q/stroke 0 160)

  (let [theta (/ (q/frame-count) 15)
        noise (* 2 (q/noise (/ theta 50)))]
    ;; add some pitch & yaw
    (q/rotate-z (q/sin (/ theta 20)))
    (q/rotate-x noise)
    ;; body
    (q/ellipsoid 5 50 5)
    (let [angle (- (* (/ Math/PI 3) (q/cos theta)) 0.4)]
      (wing angle)
      (q/rotate-y Math/PI)
      (wing (- angle)))))

(defn ^:export run-sketch []
  (q/defsketch butterfly
    :host "quil-host"
    :renderer :p3d
    :size [600 400]
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
