(ns shimmers.sketches.fire
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.macros.loop :as loop :include-macros true]
            [shimmers.math.probability :as p]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.ndarray.core :as nd]))

(defn fire-prob []
  (if (p/chance 0.05)
    (rand)
    0.0))

(defn setup []
  ;; Performance, removes calls to addType & friends
  ;; https://p5js.org/reference/#/p5/disableFriendlyErrors
  ;; still dominated by calls to fill
  (set! (.-disableFriendlyErrors js/p5) true)

  (q/background "white")
  ;; (q/frame-rate 5)
  (let [size 10
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

(defn update-fire [{:keys [fire fuel] :as state}]
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
          (when (and (> heat 0.4) (p/chance 0.25))
            (let [candidates (filterv (in-bounds xdim ydim) (surroundings x y))
                  [cx cy] (rand-nth candidates)
                  growth (nd/get-at fire cx cy)]
              (nd/set-at fire cx cy (min 1 (+ growth (* 0.3 heat)))))
            ))))
    state))

(defn update-state [state]
  (if (= (mod (inc (q/frame-count)) 1000) 0)
    (setup)
    (update-fire state)))

(defn lerp [a b x]
  (+ a (* (- b a) x)))

(defn paint [grid size color]
  (q/ellipse-mode :center)
  (apply q/fill color)
  (let [[xdim ydim] (nd/shape grid)
        hsize (/ size 2)]
    (loop/c-for [x 0 (< x xdim) (inc x)
                 y 0 (< y ydim) (inc y)]
      (let [v (nd/get-at grid x y)]
        (when (> v 0.1)
          (let [r (lerp 1 size v)]
            (q/ellipse (+ (* x size) hsize) (+ (* y size) hsize) r r)))))))

(defn draw [{:keys [fire fuel size]}]
  (paint fuel size [0 255 0 20])
  (paint fire size [255 0 0 20]))

(sketch/defquil fire
  :created-at "2020-11-08"
  :size [400 400]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
