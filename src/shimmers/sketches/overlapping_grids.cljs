(ns shimmers.sketches.overlapping-grids
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]
   [thi.ng.math.noise :as noise]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/frame-rate 8)
  {})

(defn update-state [state]
  (let [region (cq/screen-rect 0.95)]
    {:grids [{:grid (g/subdivide region {:rows 10 :cols 13})
              :stroke-weight 1.0
              :noise-threshold 0.0
              :noise-scale 0.05
              :theta 0.5
              :spacing 8}
             {:grid (g/subdivide region {:rows 6 :cols 7})
              :stroke-weight 0.7
              :noise-threshold 0.1
              :noise-scale 0.02
              :theta 1.0
              :spacing 12}
             {:grid (g/subdivide region {:rows 3 :cols 4})
              :stroke-weight 3.0
              :noise-threshold 0.2
              :noise-scale 0.05
              :theta 2.0
              :spacing 12}]}))

(defn scaled-noise [pos scale]
  (apply noise/noise2 (tm/* pos scale)))

(defn draw [{:keys [grids]}]
  (q/background 1.0)
  (q/no-fill)
  (doseq [{:keys [grid noise-threshold noise-scale
                  stroke-weight theta spacing]} grids]
    (q/stroke-weight stroke-weight)
    (doseq [r grid
            :let [center-r (g/centroid r)]]
      (cq/draw-polygon r)
      (when (> (scaled-noise center-r noise-scale) noise-threshold)
        (doseq [{[p q] :points} (clip/hatch-rectangle r spacing theta)]
          (q/line p q))))))

(sketch/defquil overlapping-grids
  :created-at "2021-11-07"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
