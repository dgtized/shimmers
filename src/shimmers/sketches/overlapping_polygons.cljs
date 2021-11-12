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
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.geom.triangle :as gt]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:pairs [[(rect/rect) (rect/rect 0.1 0.1 1)]
           [(rect/rect) (gc/circle 0.1 0.1 0.5)]
           [(rect/rect) (gt/triangle2 [0.1 0.1] [0.2 1.2] [0.9 0.8])]]})

(defn update-state [{:keys [pairs] :as state}]
  (->> pairs
       (mapv (fn [[a b]]
               (let [c (g/clip-with (g/as-polygon a) (g/as-polygon b))]
                 [a b (g/translate c (gv/vec2 1.33 0))])))
       (mapv gg/group)
       (gg/tile-grid (cq/screen-rect 0.9))
       (assoc state :shapes)))

(defn draw [{:keys [shapes]}]
  (q/ellipse-mode :radius)
  (q/no-fill)
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
