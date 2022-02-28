(ns shimmers.sketches.zigzag
  (:require [quil.core :as q :include-macros true]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch]))

(defn seconds-since-epoch []
  (/ (.getTime (js/Date.)) 1000.0))

(defn polar-noise [angle]
  (q/noise (+ (q/cos angle) 1)
           (+ (q/sin angle) 1)))

(defn zig [gap [x y] t]
  (when (and (> x 0) (< y (q/height)))
    (let [step-size (* gap (/ 9 16))
          h (- gap step-size)
          a (+ (/ (+ x y) 32) t)
          jitter (* h (polar-noise a))
          nx (- x step-size jitter)
          ny (+ y step-size jitter)]
      (q/line x y nx y)
      (q/line nx y nx ny)
      (recur gap [nx ny] t))))

(defn draw []
  (q/background 255 32)
  (q/stroke 32 16)
  (q/stroke-weight 0.6)
  (let [t (/ (seconds-since-epoch) 10)
        m (q/map-range (q/cos (/ t 4)) -1 1 16 64)
        gap (/ (q/width) (/ m 2))]
    ;; (q/print-every-n-millisec 200 (str t " " m))
    (dotimes [i m]
      (zig gap [(* i gap) 0] (/ t 256)))))

(sketch/defquil zigzag
  :created-at "2020-12-04"
  :size [600 600]
  :draw draw
  :middleware [framerate/mode])

