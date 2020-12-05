(ns shimmers.sketches.hexaclock
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.framerate :as framerate]
            [shimmers.math.vector :as v]))

(defn angles []
  (for [spur (range 0 6)]
    (/ (- (* spur 60 Math/PI) 90) 180)))

(defn spur [radius angle]
  [(* radius (q/cos angle))
   (* radius (q/sin angle))])

(defn draw []
  (q/background 255 32)
  (q/stroke 32 16)
  (q/stroke-weight 1)
  (let [s (q/seconds)
        cx (/ (q/width) 2)
        cy (/ (q/height) 2)
        radius (/ (q/width) 3)]
    (q/translate cx cy)
    (doseq [idx (range 0 6)]
      (q/line (spur radius (nth (angles) (mod idx 6)))
              (spur radius (nth (angles) (mod (inc idx) 6)))))))

(defn ^:export run-sketch []
  (q/defsketch hexaclock
    :host "quil-host"
    :size [600 600]
    :draw draw
    :middleware [framerate/mode]))


