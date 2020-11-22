(ns shimmers.dithering
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.macros.loop :as loop]
            [shimmers.framerate :as framerate]))

(def width 320)
(def height 240)

(defn setup []
  (let [applet (quil.sketch/current-applet)
        capture (.createCapture applet "video")]
    (.size capture width height)
    (.hide capture)
    {:capture capture}))

(defn closest-color [color]
  (* 256 (q/round (/ color 256))))

(defn idx [x y width]
  (* 4 (+ x (* y width))))

(defn read [pixels x y]
  (aget pixels (idx x y width)))

(defn write [pixels x y v]
  (let [i (idx x y width)]
    (aset pixels (+ i 0) v)
    (aset pixels (+ i 1) v)
    (aset pixels (+ i 2) v)
    (aset pixels (+ i 3) 255)))

(defn propagate [source x y error]
  (let [i (idx x y width)]
    (aset source i (+ error (aget source i)))))

;; https://en.wikipedia.org/wiki/Floyd%E2%80%93Steinberg_dithering
(defn dither [capture]
  (let [image (q/create-image width height)
        source(q/pixels capture)
        target (q/pixels image)]
    (dotimes [y height]
      (dotimes [x width]
        (let [old (read source x y)
              new (closest-color old)
              error (- old new)]
          (write target x y new)
          (propagate source (inc x) y (* error (/ 7 16)))
          (propagate source (dec x) (inc y) (* error (/ 3 16)))
          (propagate source x (inc y) (* error (/ 5 16)))
          (propagate source (inc x) (inc y) (* error (/ 1 16))))))
    (q/update-pixels image)
    image))

(defn draw [{:keys [capture]}]
  (q/background 255)
  (q/image (dither capture) 0 0 (* width 2) (* height 2))
  ;; (q/image capture (+ 10 width) 0)
  (framerate/display (q/current-frame-rate)))

(defn ^:export run-sketch []
  (q/defsketch shimmers
    :host "quil-host"
    :size [640 480]
    :setup setup
    :draw draw
    :middleware [m/fun-mode]))
