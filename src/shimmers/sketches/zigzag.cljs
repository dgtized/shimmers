(ns shimmers.sketches.zigzag
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.framerate :as framerate]))

(defn setup []
  (q/frame-rate 10)
  {})

(defn zig [[x y]]
  (q/stroke-weight (+ (* 0.8 (rand)) 0.2))
  (if (and (> x 0) (< y (q/height)))
    (let [nx (- x 10 (rand))
          ny (+ y 10 (rand))]
      (q/line x y nx y)
      (q/line nx y nx ny)
      (recur [nx ny]))))

(defn draw [state]
  (q/background 255)
  (q/stroke 0 100)
  (let [m 50
        o (/ (q/width) (/ m 2))]
    (dotimes [i m]
      (zig [(* i o) 0]))))

(defn ^:export run-sketch []
  (q/defsketch zigzag
    :host "quil-host"
    :size [400 400]
    :setup setup
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

