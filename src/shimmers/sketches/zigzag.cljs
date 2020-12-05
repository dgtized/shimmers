(ns shimmers.sketches.zigzag
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.framerate :as framerate]))

(defn polar-noise [angle]
  (q/noise (+ (q/cos angle) 1)
           (+ (q/sin angle) 1)))

(defn setup []
  (q/frame-rate 30)
  {})

(defn zig [[x y] t]
  ;; (q/stroke-weight (+ (* 0.8 (rand)) 0.2))
  (if (and (> x 0) (< y (q/height)))
    (let [c 8
          h 12
          a (+ (+ x y) t)
          jitter (* h (polar-noise a))
          nx (- x c jitter)
          ny (+ y c jitter)]
      (q/line x y nx y)
      (q/line nx y nx ny)
      (recur [nx ny] t))))

(defn draw [state]
  (q/background 255 20)
  (q/stroke 0 10)
  (let [m 50
        o (/ (q/width) (/ m 2))
        t (/ (q/frame-count) 300)]
    (dotimes [i m]
      (zig [(* i o) 0] t))))

(defn ^:export run-sketch []
  (q/defsketch zigzag
    :host "quil-host"
    :size [400 400]
    :setup setup
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

