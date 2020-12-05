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
  (let [angles (spur-angles)
        full (q/floor amt)]
    (doseq [idx (range 0 6)
            :while (< idx full)]
      (q/line (spur radius (nth angles idx))
              (spur radius (nth angles (mod (inc idx) 6)))))
    (let [delta (- amt full)
          [x y] (spur radius (nth angles full))
          [x' y'] (spur radius (nth angles (mod (inc full) 6)))]
      (q/line x y (q/lerp x x' delta) (q/lerp y y' delta)))))

(defn draw []
  (q/background 255 32)
  (let [sec (q/map-range (q/seconds) 0 60 0 6)
        min (q/map-range (q/minute) 0 60 0 6)
        hour (q/map-range (q/hour) 0 24 0 6)
        cx (/ (q/width) 2)
        cy (/ (q/height) 2)
        rH (/ (q/width) (Math/sqrt 5))
        rM (/ (q/width) (Math/sqrt 8))
        rS (/ (q/width) (Math/sqrt 13))]
    (q/translate cx cy)
    (q/stroke 0 0 200 64)
    (q/stroke-weight 4)
    (hexagon rS sec)
    (q/stroke 0 200 0 64)
    (q/stroke-weight 8)
    (hexagon rM min)
    (q/stroke 200 0 0 64)
    (q/stroke-weight 16)
    (hexagon rH hour)))

(defn ^:export run-sketch []
  (q/defsketch hexaclock
    :host "quil-host"
    :size [600 600]
    :draw draw
    :middleware [framerate/mode]))


