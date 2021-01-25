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

(def wing-points
  [[0 -10]
   [0 -40]
   [75 -70]
   [100 -60]
   [90 -20]
   [70 0]
   [25 -10]
   [0 -10]
   [0 -10]
   [0 40]
   [0 -5]
   [50 0]
   [60 10]
   [40 40]
   [30 60]
   [0 40]
   [0 40]])

(defn wing [angle]
  (q/push-matrix)
  (q/fill 0 8)
  ;; TODO: experiment with real lighting?
  ;; (q/shininess 100)
  ;; (q/specular 200)
  (q/rotate-y angle)
  (q/begin-shape)
  (doseq [[x y] wing-points]
    (q/curve-vertex x y 0))
  (q/end-shape :close)
  (q/pop-matrix))

(defn butterfly [theta]
  (q/ellipsoid 5 50 5)
  (let [angle (- (* (/ Math/PI 3) (q/cos theta)) 0.4)]
    (wing angle)
    (q/rotate-y Math/PI)
    (wing (- angle))))

(defn draw [_]
  (q/background 255)
  (q/stroke 0 160)
  (q/ortho)

  (let [theta (/ (q/frame-count) 15)]
    (q/with-translation [-130 120 0]
      (butterfly theta))

    (q/with-translation [-130 -70 0]
      (q/rotate-x (/ Math/PI 2.2))
      (butterfly theta))

    (q/with-translation [130 0 0]
      (let [noise (* 2 (q/noise (/ theta 50)))]
        (q/rotate-z (q/sin (/ theta 20)))
        (q/rotate-x noise))
      (butterfly theta))))

(defn ^:export run-sketch []
  (q/defsketch butterfly-sketch
    :host "quil-host"
    :renderer :p3d
    :size [600 400]
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
