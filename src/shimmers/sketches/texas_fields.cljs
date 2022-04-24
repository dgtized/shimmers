(ns shimmers.sketches.texas-fields
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [thi.ng.geom.rect :as rect]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn grid []
  (for [j (tm/norm-range 15)
        i (tm/norm-range 20)]
    (rect/rect (rv i j) (tm/+ (rv i j) (gv/vec2 (/ width 20) (/ height 15))))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.5}
            (apply list (grid))))

(sketch/definition texas-fields
  {:created-at "2022-"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :texas-fields)
              "sketch-host"))
