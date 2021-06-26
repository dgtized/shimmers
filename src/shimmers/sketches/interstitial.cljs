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

(def divisions 10)

(defn draw [state]
  (let [dwidth (/ (q/width) divisions)]
    (dotimes [i divisions]
      (dotimes [j divisions]
        (q/rect (* i dwidth) (* j dwidth) dwidth dwidth)))))

(sketch/defquil interstitial
  :created-at "2021-06-26"
  :size [800 800]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
