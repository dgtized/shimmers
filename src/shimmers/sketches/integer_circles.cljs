(ns shimmers.sketches.integer-circles
  "The Minsky Circle algorithm as discussed in:
  https://www.onirom.fr/ica.html
  https://nbickford.wordpress.com/2011/04/03/the-minsky-circle-algorithm/
  https://blog.hrvoje.org/2020/05/drawing-circles/
  https://www.shadertoy.com/view/4lSGRG"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.vector :as gv]
            [shimmers.common.quil :as cq]))

(defn next-point [[x y]]
  (let [x' (int (+ x (bit-shift-right y 4)))]
    [x' (int (- y (bit-shift-right x' 4)))]))

;; not looping yet
(defn next-point2 [d e [x y]]
  (let [x' (- x (* d y))]
    [x' (+ y (* e x'))]))

(defn iterate-until-looped [f initial]
  (loop [value initial steps []]
    (let [v (f value)]
      (if (= v initial)
        steps
        (recur v (conj steps v))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:shader (q/load-shader "shaders/integer-circles.frag.c"
                          "shaders/integer-circles.vert.c")})

(defn draw [{:keys [shader]}]
  (let [[w h] [(q/width) (q/height)]]
    (when (q/loaded? shader)
      (q/shader shader)
      (q/set-uniform shader "u_resolution" (array w h))
      (q/set-uniform shader "u_time" (/ (q/millis) 1000.0))
      (q/rect 0 0 w h))))

(sketch/defquil integer-circles
  :created-at "2021-09-04"
  :size [800 600]
  :renderer :p3d
  :setup setup
  :draw draw
  :middleware [m/fun-mode framerate/mode])
