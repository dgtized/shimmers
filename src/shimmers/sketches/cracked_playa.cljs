(ns shimmers.sketches.cracked-playa
  (:require
   [shimmers.algorithm.delaunay :as delvor]
   [shimmers.algorithm.poisson-disc-sampling :as pds]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [thi.ng.math.noise :as noise]))

(def width 900)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn noise-at-point [p]
  (let [[x y] (tm/* p 0.01)]
    (tm/clamp01 (noise/noise2 x y))))

;; TODO: add rough edges to each polygon?
;; TODO: look at smoothing polygons first, but gp/smooth does something else
(defn shapes []
  (let [bounds (rect/rect 0 0 width height)
        points (pds/generate-dynamic bounds 10 [18 256] noise-at-point)
        cells (delvor/voronoi-cells points bounds)]
    (->> (for [cell cells
               :let [width (dr/random -0.5 -4)
                     inset (gp/polygon2 (gp/inset-polygon (:points cell) width))]
               ;;:when (poly-detect/self-intersecting? inset)
               ]
           (poly-detect/split-self-intersection inset))
         (apply concat)
         (filter (fn [s] (> (g/area s) 0))))))

(defonce defo (debug/state))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 0.5}
            (debug/time-it defo [:render-time] (apply list (shapes)))))

(defn time-view []
  [:div "Rendered in " (:render-time @defo)])

(sketch/definition cracked-playa
  {:created-at "2022-04-03"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount (view-sketch/page-for scene :cracked-playa time-view)
              "sketch-host"))
