(ns shimmers.sketches.interstitial
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn square [center radius]
  (let [half (* radius 0.49)
        extent (gv/vec2 half half)]
    (rect/rect (tm/- center extent) (tm/+ center extent))))

(defn setup []
  (q/noise-seed (rand-int 1000000))
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn grid [x y width divisions]
  (let [dwidth (/ width divisions)]
    (dotimes [i divisions]
      (dotimes [j divisions]
        (q/rect (+ x (* i dwidth)) (+ y (* j dwidth)) dwidth dwidth)))))

(defn draw [state]
  (q/no-fill)
  (q/background 1.0)
  (let [divisions 8
        dwidth (/ (q/width) divisions)]
    (dotimes [i divisions]
      (dotimes [j divisions]
        (let [x (* i dwidth)
              y (* j dwidth)]
          (grid x y dwidth (Math/pow 2 (q/floor (* 6 (q/noise x y))))))))))

(sketch/defquil interstitial
  :created-at "2021-06-26"
  :size [800 800]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
