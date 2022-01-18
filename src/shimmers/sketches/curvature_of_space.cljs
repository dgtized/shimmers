(ns shimmers.sketches.curvature-of-space
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [thi.ng.geom.svg.core :as svg]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes []
  (svg/group {:transform (csvg/translate (rv 0.5 0.5))}
             (csvg/path [[:M (gv/vec2)]
                         [:A [30 30] 0.0 0 1 (gv/vec2 20 20)]
                         [:A [15 15] 0.0 1 0 (gv/vec2 0 20)]
                         [:A [40 40] 0.0 1 0 (gv/vec2 -20 20)]])))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 1.5}
            (shapes)))

(sketch/definition curvature-of-space
  {:created-at "2022-01-18"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :curvature-of-space)
              "sketch-host"))
