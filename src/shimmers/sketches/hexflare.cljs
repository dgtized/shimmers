(ns shimmers.sketches.hexflare
  (:require
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
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

(defn rp [r theta]
  (v/polar (* r 0.5 height) theta))

(defn shapes []
  (let [r (* height 0.05)
        line (gl/line2 (rv 0.0 0.9) (rv 1.0 0.05))
        dist (tm/mag line)
        hexes (for [t (dr/density-range (/ (* 1.1 (Math/sqrt 3) r) dist)
                                        (/ (* 4 r) dist))]
                (g/as-polygon (gc/circle (g/point-at line t) r) 6))
        center (apply gp/polygon2 (gp/inset-polygon (g/vertices (cs/middle hexes)) 5))
        ]
    (into hexes [center line])))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.5}
            (shapes)))

(sketch/definition hexflare
  {:created-at "2021-12-30"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount (view-sketch/page-for scene :hexflare)
              "sketch-host"))
