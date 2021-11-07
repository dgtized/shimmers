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
  (let [region (cq/screen-rect 0.95)]
    {:grids [(g/subdivide region {:rows 10 :cols 13})]}))

(defn update-state [state]
  state)

(defn draw [{:keys [grids]}]
  (q/background 1.0)
  (doseq [grid grids]
    (doseq [r grid
            :let [center-r (g/centroid r)]]
      (cq/draw-polygon r)
      (when (> (apply noise/noise2 (tm/* center-r 0.5)) 0.2)
        (doseq [{[p q] :points} (clip/hatch-rectangle r 8 0.5)]
          (q/line p q))))))

(sketch/defquil overlapping-grids
  :created-at "2021-11-07"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
