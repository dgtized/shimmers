(ns shimmers.sketches.hexflare
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
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
        line (gl/line2 (rv 0.0 0.9) (rv 1.0 0.05))]
    (into (for [t (tm/norm-range (/ (apply g/dist (:points line)) (*  (Math/sqrt 3) r)))]
            (g/as-polygon (gc/circle (g/point-at line t) r) 6))
          [line])))

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
