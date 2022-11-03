(ns shimmers.sketches.spiderwebs
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes []
  (let [center (rv 0.5 0.5)
        radius (* 0.4 height)
        points (g/vertices (gc/circle center radius) (dr/random-int 15 23))]
    (conj (for [p points]
            (gl/line2 center p))
          (gl/linestrip2
           (map (fn [point r] (tm/mix center point r))
                (cycle points)
                (drop 16 (dr/density-range 0.0005 0.003)))))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.5}
    (shapes)))

(sketch/definition spiderwebs
  {:created-at "2022-11-03"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :spiderwebs)
              "sketch-host"))
