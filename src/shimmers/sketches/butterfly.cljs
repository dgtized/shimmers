(ns shimmers.sketches.butterfly
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]))

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

(defn wing [wing-shape angle]
  (q/push-matrix)
  (q/fill 0 8)
  ;; TODO: experiment with real lighting?
  ;; (q/shininess 100)
  ;; (q/specular 200)
  (q/rotate-y angle)
  (q/begin-shape :lines)
  (doseq [[x y] wing-shape]
    (if (zero? x)
      (q/curve-vertex x y 0)
      (q/curve-vertex x y (* 20 (math/sin angle)))))
  (q/end-shape :close)
  (q/pop-matrix))

(defn make-butterfly [wing-shape]
  (fn [theta]
    (q/ellipsoid 5 50 5)
    (let [angle (q/lerp (- (* math/PI (/ 70 128))) (/ math/PI 3)
                        (/ (+ 1 (math/cos theta)) 2))]
      (wing wing-shape angle)
      (q/rotate-y math/PI)
      (wing wing-shape (- angle)))))

(defn draw [_]
  (q/background 255)
  (q/stroke 0 160)
  (q/ortho)

  (let [butterfly (make-butterfly wing-points)
        theta (/ (q/frame-count) 15)]
    (q/with-translation [-130 120 0]
      (butterfly theta))

    (q/with-translation [-130 -70 0]
      (q/rotate-x (/ math/PI 2.2))
      (butterfly theta))

    (q/with-translation [130 0 0]
      (let [noise (* 2 (q/noise (/ theta 50)))]
        (q/rotate-z (math/sin (/ theta 20)))
        (q/rotate-x noise))
      (butterfly theta))))

(defn page []
  (sketch/component
   :size [600 400]
   :renderer :p3d
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition butterfly
  {:created-at "2021-01-25"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
