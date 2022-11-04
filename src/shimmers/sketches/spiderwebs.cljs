(ns shimmers.sketches.spiderwebs
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes []
  (let [center (rv 0.5 0.5)
        radius (* 0.4 height)
        points (mapv (fn [p] (tm/+ p (dr/jitter (* 0.08 radius))))
                     (g/vertices (gc/circle center radius) (dr/random-int 15 23)))
        center-r 0.02]
    (conj (for [p points]
            (gl/line2 (tm/mix center p center-r) p))
          (gp/polygon2 (map #(tm/mix center % center-r) points))
          (gl/linestrip2
           (map (fn [point r] (tm/mix center point r))
                (cycle points)
                (drop-while #(< % center-r) (dr/density-range 0.0001 0.0025)))))))

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
