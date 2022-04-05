(ns shimmers.sketches.noise-grid
  "Display a tiling grid of noise function to show dicontinuities."
  (:require
   [quil.core :as q :include-macros true]
   [quil.sketch]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.math.core :as sm]
   [shimmers.sketch :as sketch :include-macros true]))

(defn noise-at [x y _ factor fc]
  (q/noise (/ x factor) (/ y factor) fc))

(defn reflect-noise-at [x y size factor fc]
  (let [qx (sm/reflect-into x size)
        qy (sm/reflect-into y size)]
    (q/noise (/ qx factor) (/ qy factor) fc)))

(defn draw-square [size factor noise-fn]
  (let [fc (/ (q/frame-count) factor)]
    (dotimes [y (inc size)]
      (dotimes [x (inc size)]
        (q/stroke (* 255 (noise-fn x y size factor fc)))
        (q/point x y)))))

(defonce ui
  (atom {:reflect false
         :factor 10}))

(defn setup []
  (q/frame-rate 10)
  (let [size (int (/ (q/width) 3))
        applet (quil.sketch/current-applet)
        reflect (.createCheckbox applet "Reflect Tile" (:reflect @ui))
        factor (.createSlider applet 2 64 (:factor @ui) 2)
        _ (.createSpan applet "Factor")]
    (.changed reflect (fn [] (swap! ui assoc :reflect (.checked reflect))))
    (.changed factor (fn [] (swap! ui assoc :factor (.value factor))))
    {:size size
     :tile (q/create-graphics size size)}))

(defn draw [{:keys [size tile]}]
  (q/background "white")
  (let [{:keys [reflect factor]} @ui]
    (q/with-graphics tile
      (draw-square
       size
       factor
       (if reflect reflect-noise-at noise-at)))
    (dotimes [gy 3]
      (dotimes [gx 3]
        (q/image tile (* gx size) (* gy size))))))

(sketch/defquil noise-grid
  :created-at "2020-11-01"
  :tags #{:demo}
  :size [300 300]
  :setup setup
  :draw draw
  :middleware [m/fun-mode framerate/mode])
