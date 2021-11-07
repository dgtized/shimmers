(ns shimmers.sketches.overlapping-grids
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/noise-seed (dr/random-int 100000))
  (q/color-mode :hsl 1.0)
  (q/no-loop)
  (let [region (cq/screen-rect 0.95)
        theta (dr/random 0.6 1.2)]
    {:grids [{:grid (dr/random-sample 0.85 (g/subdivide region {:rows 10 :cols 13}))
              :stroke-weight 1.0
              :cell-color [0.99 0.5 0.5 0.5]
              :noise-threshold 0.5
              :noise-scale 0.05
              :theta theta
              :spacing 8}
             {:grid (dr/random-sample 0.90 (g/subdivide region {:rows 6 :cols 7}))
              :stroke-weight 0.7
              :cell-color [0.6 0.8 0.5 1.0]
              :noise-threshold 0.6
              :noise-scale 0.03
              :theta (+ theta (dr/random 0.5 1.0))
              :spacing 12}
             {:grid (dr/random-sample 0.95 (g/subdivide region {:rows 3 :cols 4}))
              :stroke-weight 3.0
              :cell-color [0.05 0.6 0.5 0.5]
              :noise-threshold 0.75
              :noise-scale 0.02
              :theta (+ theta (dr/random 1.0 2.0))
              :spacing 12}]}))

(defn update-state [state]
  state)

(defn scaled-noise [pos scale]
  (let [[x y] (tm/* pos scale)]
    (q/noise x y)))

(defn draw [{:keys [grids]}]
  (q/background 1.0)
  (q/no-fill)
  (doseq [{:keys [grid noise-threshold noise-scale
                  stroke-weight cell-color
                  theta spacing]} grids]
    (apply q/stroke cell-color)
    (q/stroke-weight stroke-weight)
    (doseq [r grid
            :let [center-r (g/centroid r)]]
      (cq/draw-polygon r)
      (q/stroke-weight (* 0.5 stroke-weight))
      (when (> (scaled-noise center-r noise-scale) noise-threshold)
        (doseq [{[p q] :points} (clip/hatch-rectangle r spacing theta [0.5 0.5])]
          (q/line p q))))))

(sketch/defquil overlapping-grids
  :created-at "2021-11-07"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
