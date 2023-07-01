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
        n-points (dr/random-int 15 23)
        points (mapv (fn [p] (tm/+ p (dr/jitter (* 0.08 radius))))
                     (g/vertices (gc/circle center radius) n-points))
        center-r 0.02
        center-circle (gp/polygon2 (map #(tm/mix center % center-r) points))
        spiral (->> (dr/gaussian-range 0.0015 0.0015)
                    (drop-while #(< % center-r))
                    (map (fn [point r] (tm/mix center point r))
                         (cycle points)))

        radial-lines
        (->> spiral
             (map-indexed vector)
             (reduce (fn [lines [i p]]
                       (update lines (mod i n-points)
                               (fnil (fn [pts] (conj pts p)) [])))
                     {})
             vals
             (map gl/linestrip2))]
    (conj radial-lines
          center-circle
          (gl/linestrip2 spiral))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.5}
    (shapes)))

(sketch/definition spiderwebs
  {:created-at "2022-11-03"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/static-page scene :spiderwebs)))
