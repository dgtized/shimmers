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
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]))

;; Interested in operations for calculating resulting polygons after punching
;; out an overlap. So for two squares, return the clip intersection + the two
;; bracketing polygons, or for the circle/square the intersection, and then
;; pacman and remainder of the square.

;; Possibly algorithms
;; https://www.researchgate.net/publication/222450704_An_algorithm_for_computing_the_union_intersection_or_difference_of_two_polygons
;; https://core.ac.uk/download/pdf/82372399.pdf

(defn setup []
  (q/color-mode :hsl 1.0)
  {:pairs [[(rect/rect) (rect/rect 0.1 0.1 1)]
           ;; Can we detect the arc segment and keep that as an arc instead of using small lines
           [(rect/rect) (gc/circle 0.1 0.1 0.5)]
           [(rect/rect) (gt/triangle2 [0.1 0.1] [0.2 1.2] [0.9 0.8])]
           ;; Note this *only* works for convex polygons, weird results otherwise
           [(rect/rect) (gp/polygon2 [[0.1 0.1] [0.5 0.1] [1.1 0.5] [0.8 1.1] [-0.2 0.5]])]]})

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
