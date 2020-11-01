(ns shimmers.noise-grid
  "Display a tiling grid of noise function to show dicontinuities."
  (:require [quil.core :as q :include-macros true]
            [shimmers.framerate :as framerate]
            [quil.middleware :as m]))

(defn noise-grid [x y _]
  (let [factor 10]
    (q/noise (/ x factor) (/ y factor)
             (/ (q/frame-count) factor))))

(defn reflect-into [v size]
  (let [reflection (/ size 2)
        v (mod v size)]
    (cond (< v reflection)
          v
          (>= v reflection)
          (- size v))))

(defn noise-tile [x y size]
  (let [factor 10]
    (let [qx (reflect-into x size)
          qy (reflect-into y size)]
      (q/noise (/ qx factor) (/ qy factor)
               (/ (q/frame-count) factor)))))

(defn draw-square [size noise-fn]
  (dotimes [y size]
    (dotimes [x size]
      (q/stroke (* 255 (noise-fn x y size)))
      (q/point x y))))

(defn setup []
  (q/frame-rate 10)
  (let [ui (atom {:reflect false})
        reflect (.createCheckbox (quil.sketch/current-applet) "Reflect Tile" (:reflect @ui))]
    (.changed reflect (fn [] (swap! ui assoc :reflect (.checked reflect))))
    ui))

(defn draw [ui]
  (q/background "white")
  (let [size 100
        tile (q/create-graphics size size)]
    (q/with-graphics tile
      (draw-square size (if (:reflect @ui) noise-tile noise-grid)))
    (dotimes [gy 3]
      (dotimes [gx 3]
        (q/image tile (* gx size) (* gy size))))
    (framerate/display (q/current-frame-rate))))

(defn ^:export run-sketch []
  (q/defsketch particles
    :host "quil-host"
    :size [300 300]
    :setup setup
    :draw draw
    :middleware [m/fun-mode]))
