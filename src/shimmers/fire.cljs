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
  (q/frame-rate 5)
  (let [size 20
        w    (/ (q/width) size)
        h    (/ (q/height) size)]
    {:size size
     :fire (nd/ndarray :float64 (repeatedly (* w h) #(fire-prob)) [w h])
     :fuel (nd/ndarray :float64 (repeatedly (* w h) #(rand)) [w h])}))

(defn in-bounds [limit-x limit-y]
  (fn [[x y]] (and (>= x 0) (< x limit-x)
                  (>= y 0) (< y limit-y))))

(defn surroundings [x y]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not= 0 dx dy)]
    [(+ x dx) (+ y dy)]))

(defn update-state [{:keys [fire fuel size] :as state}]
  (let [[xdim ydim] (nd/shape fire)]
    (loop/c-for [x 0 (< x xdim) (inc x)
                 y 0 (< y ydim) (inc y)]
      (let [heat (nd/get-at fire x y)
            wood (nd/get-at fuel x y)]
        (when (> heat 0.1)
          (if (> wood 0.2)
            (do (nd/set-at fire x y (min (* heat 1.05) 1))
                (nd/set-at fuel x y (* wood 0.96)))
            (do (nd/set-at fire x y (min (* heat 0.75) 1))
                (nd/set-at fuel x y (* wood 0.99))))
          (when (and (> heat 0.6) (> (rand) 0.6))
            (let [candidates (filterv (in-bounds xdim ydim) (surroundings x y))
                  [cx cy] (rand-nth candidates)
                  cheat (nd/get-at fire cx cy)]
              (nd/set-at fire cx cy (+ cheat 0.5)))
            )))))
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
  (q/background "black")
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
