(ns shimmers.fire
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.framerate :as framerate]
            [shimmers.math.vector :as v]
            [shimmers.macros.loop :as loop :include-macros true]
            [thi.ng.ndarray.core :as nd]))

(defn fire-prob []
  (if (> (rand) 0.95)
    (rand)
    0.0))

(defn setup []
  (q/background "black")
  (let [size 10
        w    (/ (q/width) size)
        h    (/ (q/height) size)]
    {:size size
     :fire (nd/ndarray :float64 (repeatedly (* w h) #(fire-prob)) [w h])
     :fuel (nd/ndarray :float64 (repeatedly (* w h) #(rand)) [w h])}))

(defn update-state [state]
  state)

(defn paint [grid size color]
  (q/ellipse-mode :corner)
  (let [[xdim ydim] (nd/shape grid)
        hsize (/ size 2)]
    (loop/c-for [x 0 (< x xdim) (inc x)
                 y 0 (< y ydim) (inc y)]
      (let [v (nd/get-at grid x y)]
        (when (> v 0.1)
          (apply q/stroke color)
          (q/ellipse (* x size) (* y size) (q/lerp 1 size v) (q/lerp 1 size v)))))))

(defn draw [{:keys [fire fuel size]}]
  (paint fuel size [0 255 0 255])
  (paint fire size [255 0 0 255])
  (framerate/display (q/current-frame-rate)))

(defn ^:export run-sketch []
  (q/defsketch fire
    :host "quil-host"
    :size [400 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode]))
