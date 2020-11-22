(ns shimmers.dithering
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.macros.loop :as loop]
            [shimmers.framerate :as framerate]))

(defn setup []
  (let [width 320
        height 240
        applet (quil.sketch/current-applet)
        capture (.createCapture applet "video")]
    (.size capture width height)
    (.hide capture)
    {:width width
     :height height
     :capture capture}))

(defn closest-color [color]
  (* 256 (q/round (/ color 256))))

(defn idx [x y width]
  (* 4 (+ x (* y width))))

(defn write [pixels i v]
  (aset pixels (+ i 0) v)
  (aset pixels (+ i 1) v)
  (aset pixels (+ i 2) v)
  (aset pixels (+ i 3) 255))

(defn propagate [source i error]
  (aset source i (+ error (aget source i))))

;; https://en.wikipedia.org/wiki/Floyd%E2%80%93Steinberg_dithering
(defn dither [capture width height]
  (let [image (q/create-image width height)
        source(q/pixels capture)
        target (q/pixels image)]
    (dotimes [y height]
      (dotimes [x width]
        (let [i (idx x y width)
              old (aget source i)
              new (closest-color old)
              error (- old new)]
          (write target i new)
          (propagate source (idx (inc x) y width)
                     (* error (/ 7 16)))
          (propagate source (idx (dec x) (inc y) width)
                     (* error (/ 3 16)))
          (propagate source (idx x (inc y) width)
                     (* error (/ 5 16)))
          (propagate source (idx (inc x) (inc y) width)
                     (* error (/ 1 16))))))
    (q/update-pixels image)
    image))

(defn draw [{:keys [capture width height]}]
  (q/background 255)
  (q/image (dither capture width height) 0 0 (* width 2) (* height 2))
  ;; (q/image capture (+ 10 width) 0)
  (framerate/display (q/current-frame-rate)))

(defn ^:export run-sketch []
  (q/defsketch shimmers
    :host "quil-host"
    :size [640 480]
    :setup setup
    :draw draw
    :middleware [m/fun-mode]))
