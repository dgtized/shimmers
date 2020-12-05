(ns shimmers.sketches.hexaclock
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.framerate :as framerate]
            [shimmers.math.vector :as v]))

(defn spur-angles []
  (for [spur (range 0 6)]
    (- (/ (* spur 60 Math/PI) 180)
       (/ Math/PI 2))))

(defn spur [radius angle]
  [(* radius (q/cos angle))
   (* radius (q/sin angle))])

(defn hexagon [radius amt]
  (let [angles (spur-angles)]
    (doseq [idx (range 0 6)
            :while (< idx amt)]
      (q/line (spur radius (nth angles idx))
              (spur radius (nth angles (mod (inc idx) 6)))))))

(defn draw []
  (q/background 255 32)
  (q/stroke 32 16)
  (let [sec (q/map-range (q/seconds) 0 60 0 6)
        min (q/map-range (q/minute) 0 60 0 6)
        hour (q/map-range (q/hour) 0 24 0 6)
        cx (/ (q/width) 2)
        cy (/ (q/height) 2)
        rH (/ (q/width) (Math/sqrt 5))
        rM (/ (q/width) (Math/sqrt 6))
        rS (/ (q/width) (Math/sqrt 7))]
    (q/translate cx cy)
    (q/stroke-weight 1)
    (hexagon rS sec)
    (q/stroke-weight 2)
    (hexagon rM min)
    (q/stroke-weight 4)
    (hexagon rH hour)))

(defn ^:export run-sketch []
  (q/defsketch hexaclock
    :host "quil-host"
    :size [600 600]
    :draw draw
    :middleware [framerate/mode]))


