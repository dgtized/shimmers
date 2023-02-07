(ns shimmers.math.control
  (:require [shimmers.math.equations :as eq]
            [thi.ng.math.core :as tm]))

(defn angular-delta [angle target]
  (let [delta (- target angle)]
    (cond (< delta (- Math/PI)) (+ delta eq/TAU)
          (> delta Math/PI) (- delta eq/TAU)
          :else delta)))

;; see also https://gamedev.stackexchange.com/questions/1885/target-tracking-when-to-accelerate-and-decelerate-a-rotating-turret
(defn angular-acceleration [angle target control angle-vel]
  (let [delta (angular-delta angle target)]
    (- (* control delta)
       (* (* 2 (Math/sqrt control)) angle-vel))))

(defn force-accel [pos target control velocity]
  (let [dir (tm/- target pos)]
    (tm/- (tm/* dir control)
          (tm/* velocity (* 2 (Math/sqrt control))))))
