(ns shimmers.sketches.integer-circles
  "The Minsky Circle algorithm as discussed in:
  https://www.onirom.fr/ica.html
  https://nbickford.wordpress.com/2011/04/03/the-minsky-circle-algorithm/
  https://blog.hrvoje.org/2020/05/drawing-circles/
  https://www.shadertoy.com/view/4lSGRG"
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.shader :as shader]
   [shimmers.sketch :as sketch :include-macros true]))

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
  (let [size [640 480]
        [sx sy] size]
    {:size size
     :pass1 (q/create-graphics sx sy :p3d)
     :shader (q/load-shader "shaders/integer-circles.frag.c"
                            "shaders/integer-circles.vert.c")}))

;; Multi-pass https://github.com/aferriss/p5jsShaderExamples/blob/gh-pages/4_image-effects/4-10_two-pass-blur/sketch.js
(defn draw [{:keys [size pass1 shader]}]
  (let [[w h] size
        k (int (+ 5 (mod (/ (q/frame-count) 60) 15)))
        ;; k == 6 is a blank screen so skip it
        k (if (= k 6) 21 k)]
    (when (q/loaded? shader)
      (q/with-graphics pass1
        (shader/pass shader size
                     {"u_resolution" (array w h)
                      "u_time" (/ (q/millis) 1000.0)
                      "u_d" 1.0
                      "u_e" (* 4.0 (Math/pow (Math/sin (/ Math/PI k)) 2))})))
    (q/image pass1 0 0 (q/width) (q/height))))

(sketch/defquil integer-circles
  :created-at "2021-09-04"
  :tags #{:shader}
  :size [1280 960]
  :renderer :p2d
  :setup setup
  :draw draw
  :middleware [m/fun-mode framerate/mode])
