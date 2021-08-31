(ns shimmers.sketches.noise-grid
  "Display a tiling grid of noise function to show dicontinuities."
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.core :as sm]
            [shimmers.sketch :as sketch :include-macros true]))

(defn noise-grid [x y _ factor]
  (q/noise (/ x factor) (/ y factor)
           (/ (q/frame-count) factor)))

(defn noise-tile [x y size factor]
  (let [qx (sm/reflect-into x size)
        qy (sm/reflect-into y size)]
    (q/noise (/ qx factor) (/ qy factor)
             (/ (q/frame-count) factor))))

(defn draw-square [size factor noise-fn]
  (dotimes [y size]
    (dotimes [x size]
      (q/stroke (* 255 (noise-fn x y size factor)))
      (q/point x y))))

(defn setup []
  (q/frame-rate 10)
  (let [ui (atom {:reflect false
                  :factor 10})
        applet (quil.sketch/current-applet)
        reflect (.createCheckbox applet "Reflect Tile" (:reflect @ui))
        factor (.createSlider applet 2 64 (:factor @ui) 2)
        _ (.createSpan applet "Factor")]
    (.changed reflect (fn [] (swap! ui assoc :reflect (.checked reflect))))
    (.changed factor (fn [] (swap! ui assoc :factor (.value factor))))
    ui))

(defn draw [ui]
  (q/background "white")
  (let [size 100
        tile (q/create-graphics size size)
        {:keys [reflect factor]} @ui]
    (q/with-graphics tile
      (draw-square
       size
       factor
       (if reflect noise-tile noise-grid)))
    (dotimes [gy 3]
      (dotimes [gx 3]
        (q/image tile (* gx size) (* gy size))))))

(sketch/defquil noise-grid-sketch
  :created-at "2020-11-01"
  :tags #{:demo}
  :size [300 300]
  :setup setup
  :draw draw
  :middleware [m/fun-mode framerate/mode])
