(ns shimmers.dithering
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.macros.loop :as loop]
            [shimmers.framerate :as framerate]))

(def width 256)
(def height 192)

(defn setup []
  (let [applet (quil.sketch/current-applet)
        capture (.createCapture applet "video")]
    (.size capture width height)
    (.hide capture)
    {:capture capture}))

(defn idx [x y width]
  (* 4 (+ x (* y width))))

(defn read [pixels x y]
  (aget pixels (idx x y width)))

(defn write [pixels x y v]
  (let [i (idx x y width)]
    (when (and (>= i 0) (< i (* 4 width height)))
      (aset pixels (+ i 0) v)
      (aset pixels (+ i 1) v)
      (aset pixels (+ i 2) v)
      (aset pixels (+ i 3) 255))))

;; https://en.wikipedia.org/wiki/Floyd%E2%80%93Steinberg_dithering
(defn dither [capture]
  (let [image (q/create-image width height)
        source(q/pixels capture)
        target (q/pixels image)
        size (* 4 width height)]
    (dotimes [y height]
      (dotimes [x width]
        (let [v (read source x y)]
          (write target x y v))))
    (q/update-pixels image)
    image))

(defn draw [{:keys [capture]}]
  (q/background 255)
  (q/image (dither capture) 0 0)
  (q/image capture (+ 10 width) 0)
  (framerate/display (q/current-frame-rate)))

(defn ^:export run-sketch []
  (q/defsketch shimmers
    :host "quil-host"
    :size [640 480]
    :setup setup
    :draw draw
    :middleware [m/fun-mode]))
