(ns shimmers.sketches.hexaclock
  (:require [quil.core :as q :include-macros true]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.sketch :as sketch :include-macros true]))

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
    (cq/lerp-line (spur radius (nth angles full))
                  (spur radius (nth angles (mod (inc full) 6)))
                  (- amt full))))

(defn draw []
  (q/frame-rate 6)
  (q/background 255 24)
  (let [sec (q/map-range (q/seconds) 0 60 0 6)
        minute (q/map-range (q/minute) 0 60 0 6)
        hour (q/map-range (mod (q/hour) 12) 0 12 0 6)
        cx (cq/rel-w 0.5)
        cy (cq/rel-h 0.5)
        small-radius (min cx cy)
        rH (/ small-radius 1.2)
        rM (/ small-radius 1.6)
        rS (/ small-radius 2.2)]
    (q/translate cx cy)
    (q/stroke 0 0 200 64)
    (q/stroke-weight 4)
    (hexagon rS sec)
    (q/stroke 0 200 0 64)
    (q/stroke-weight 8)
    (hexagon rM minute)
    (q/stroke 200 0 0 64)
    (q/stroke-weight 16)
    (hexagon rH hour)))

(sketch/defquil hexaclock
  :created-at "2020-12-04"
  :size [600 400]
  :draw draw
  :middleware [framerate/mode])


