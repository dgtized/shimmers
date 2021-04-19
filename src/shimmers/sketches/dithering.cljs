(ns shimmers.sketches.dithering
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [reagent.core :as r]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.common.video :as video]))

(def modes [:dither :boxes :circles :color-displace])

(defonce ui-state (r/atom {:mode :dither}))

(defn setup []
  (let [width 320
        height 240]
    (ctrl/mount (partial ctrl/change-mode ui-state modes))
    {:width width
     :height height
     :capture (video/capture width height)}))

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
        source (q/pixels capture)
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

(defn boxes [capture width height]
  (q/rect-mode :corner)
  (q/no-stroke)
  (q/fill 0)
  (let [box-size 3
        pixels (q/pixels capture)]
    (dotimes [y (/ height box-size)]
      (dotimes [x (/ width box-size)]
        (let [r (aget pixels (idx (* x box-size) (* y box-size) width))
              g (aget pixels (+ (idx (* x box-size) (* y box-size) width) 1))
              b (aget pixels (+ (idx (* x box-size) (* y box-size) width) 2))
              size (q/map-range (/ (+ r g b) 3) 0 255 (* box-size 1.75) 0.5)]
          (q/rect (* x 2 box-size) (* y 2 box-size) size size))))))

(defn circles [capture width height]
  (q/rect-mode :corner)
  (q/no-stroke)
  (q/fill 0)
  (let [box-size 3
        pixels (q/pixels capture)]
    (dotimes [y (/ height box-size)]
      (dotimes [x (/ width box-size)]
        (let [r (aget pixels (idx (* x box-size) (* y box-size) width))
              g (aget pixels (+ (idx (* x box-size) (* y box-size) width) 1))
              b (aget pixels (+ (idx (* x box-size) (* y box-size) width) 2))
              size (q/map-range (/ (+ r g b) 3) 0 255 (* box-size 1.8) 0.2)]
          (q/ellipse (* x 2 box-size) (* y 2 box-size) size size))))))

(defn color-displace [capture width height]
  (q/rect-mode :corner)
  (q/no-stroke)
  (let [box-size 4
        pixels (q/pixels capture)
        displace (/ box-size 4)
        v 200
        a 128]
    (dotimes [y (/ height box-size)]
      (dotimes [x (/ width box-size)]
        (let [r (aget pixels (idx (* x box-size) (* y box-size) width))
              g (aget pixels (+ (idx (* x box-size) (* y box-size) width) 1))
              b (aget pixels (+ (idx (* x box-size) (* y box-size) width) 2))
              rsize (q/map-range r 0 255 (* box-size 1.2) 0.2)
              gsize (q/map-range g 0 255 (* box-size 1.2) 0.2)
              bsize (q/map-range b 0 255 (* box-size 1.2) 0.2)]
          (q/fill v 0 0 255)
          (q/ellipse (+ (* x 2 box-size) displace) (+ (* y 2 box-size) displace) rsize rsize)
          (q/fill 0 v 0 255)
          (q/ellipse (+ (* x 2 box-size) (- displace)) (+ (* y 2 box-size) displace) gsize gsize)
          (q/fill 0 0 v 48)
          (q/ellipse (+ (* x 2 box-size) 0) (+ (* y 2 box-size) (- displace)) bsize bsize))))))

(defn draw [{:keys [capture width height]}]
  (q/background 255)
  (let [ui-mode (:mode (deref ui-state))]
    (case ui-mode
      :dither (q/image (dither capture width height) 0 0 (* width 2) (* height 2))
      :boxes (boxes capture width height)
      :circles (circles capture width height)
      :color-displace (color-displace capture width height)))
  ;; (q/image capture (+ 10 width) 0)
  )

(defn ^:export run-sketch []
  (q/defsketch shimmers
    :host "quil-host"
    :size [640 480]
    :setup setup
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
