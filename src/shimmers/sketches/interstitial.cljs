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
        percent (/ width (q/width))]
    (dotimes [i divisions]
      (dotimes [j divisions]
        (let [sx (+ x (* i dwidth))
              sy (+ y (* j dwidth))
              noise (q/noise (/ sx 128) (/ sy 128) (/ (q/frame-count) 800))]
          (if (> (* percent noise) 0.1)
            (grid sx sy dwidth (cond (< noise 0.2) 8
                                     (< noise 0.4) 5
                                     (< noise 0.6) 4
                                     (< noise 0.9) 3
                                     :else 2))
            (cond (< noise 0.2)
                  (q/triangle sx sy
                              sx (+ sy dwidth)
                              (+ sx dwidth) (+ sy dwidth))
                  (< noise 0.47)
                  (q/rect sx sy dwidth dwidth)
                  (< noise 0.53)
                  (q/triangle sx sy
                              (+ sx dwidth) sy
                              sx (+ sy dwidth))
                  (< noise 0.85)
                  (q/ellipse sx sy dwidth dwidth)
                  :else
                  (q/triangle sx sy
                              (+ sx dwidth) sy
                              (+ sx dwidth) (+ sy dwidth))
                  )))))))

(defn draw [state]
  (q/no-fill)
  (q/background 1.0 0.1)
  (q/stroke-weight 1.0)
  (q/stroke 0 0.2)
  (grid 0 0 (q/width) 5))

(sketch/defquil interstitial
  :created-at "2021-06-26"
  :size [800 800]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
