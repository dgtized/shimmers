(ns shimmers.sketches.overlapping-polygons
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.math.geometry.group :as gg]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.rect :as rect]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:pairs (->> [[(rect/rect) (rect/rect 0.1 0.1 1)]
                [(rect/rect) (gc/circle 0.1 0.1 0.5)]]
               (mapv gg/group))})

(defn update-state [{:keys [pairs] :as state}]
  (assoc state :shapes
         (gg/tile-grid (cq/screen-rect 0.9) pairs)))

(defn draw [{:keys [shapes]}]
  (q/ellipse-mode :radius)
  (q/background 1.0)
  (when shapes
    (qdg/draw shapes)))

(sketch/defquil overlapping-polygons
  :created-at "2021-11-12"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
