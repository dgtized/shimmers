(ns shimmers.noise-grid
  "Display a tiling grid of noise function to show dicontinuities."
  (:require [quil.core :as q :include-macros true]))

(defn noise-grid [x y]
  (let [factor 10]
    (q/noise (/ x factor) (/ y factor))))

(defn draw-square [size]
  (dotimes [y size]
    (dotimes [x size]
      (q/stroke (* 255 (noise-grid x y)))
      (q/point x y))))

(defn draw []
  (q/background "white")
  (let [size 100]
    (dotimes [gy 3]
      (dotimes [gx 3]
        (q/with-translation [(* gx size) (* gy size)]
          (draw-square size))))))

(defn ^:export run-sketch []
  (q/defsketch particles
    :host "quil-host"
    :size [300 300]
    :setup (fn [] (q/frame-rate 2))
    :draw draw))
