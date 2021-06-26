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
  (let [dwidth (/ width divisions)
        percent (/ dwidth (q/width))]
    (dotimes [i divisions]
      (dotimes [j divisions]
        (let [sx (+ x (* i dwidth))
              sy (+ y (* j dwidth))
              noise (+ percent (* 0.85 (q/noise sx sy (/ (q/frame-count) 500))))]
          (cond (< noise 0.3)
                (q/ellipse sx sy dwidth dwidth)
                (< noise 0.55)
                (q/rect sx sy dwidth dwidth)
                (< noise 0.8)
                (q/triangle sx sy (+ sx dwidth) sy sx (+ sy dwidth))
                :else
                (grid sx sy dwidth 2)))))))

(defn draw [state]
  (q/no-fill)
  (q/background 1.0)
  (grid 0 0 (q/width) 8))

(sketch/defquil interstitial
  :created-at "2021-06-26"
  :size [800 800]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
