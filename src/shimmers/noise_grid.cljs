(ns shimmers.noise-grid
  "Display a tiling grid of noise function to show dicontinuities."
  (:require [quil.core :as q :include-macros true]
            [shimmers.framerate :as framerate]))

(defn noise-grid [x y]
  (let [factor 10]
    (q/noise (/ x factor) (/ y factor)
             (/ (q/frame-count) factor))))

(defn draw-square [size]
  (dotimes [y size]
    (dotimes [x size]
      (q/stroke (* 255 (noise-grid x y)))
      (q/point x y))))

(defn draw []
  (q/background "white")
  (let [size 100
        tile (q/create-graphics size size)]
    (q/with-graphics tile
      (draw-square size))
    (dotimes [gy 3]
      (dotimes [gx 3]
        (q/image tile (* gx size) (* gy size))))
    (framerate/display (q/current-frame-rate))))

(defn ^:export run-sketch []
  (q/defsketch particles
    :host "quil-host"
    :size [300 300]
    :draw draw))
