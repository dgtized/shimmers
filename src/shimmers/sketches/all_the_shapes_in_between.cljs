(ns shimmers.sketches.all-the-shapes-in-between
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(def shape-seq
  (let [circle (gc/circle (gv/vec2) (* 0.05 height))]
    [circle
     (g/as-polygon circle 5)
     (g/bounds circle)
     (triangle/inscribed-equilateral circle 0.0)]))

(defn path [t]
  (g/point-at (gc/circle (rv 0.5 0.5) (* height 0.4)) t))

(defn morph [from to t]
  (for [v (butlast (tm/norm-range 32))]
    (tm/mix (g/point-at from v) (g/point-at to v) t)))

(defn shapes []
  (for [t (butlast (tm/norm-range 24))]
    (let [base (int (* t (count shape-seq)))]
      (g/translate (gp/polygon2 (morph (nth shape-seq base)
                                       (nth shape-seq (mod (inc base) (count shape-seq)))
                                       (mod (* t (count shape-seq)) 1.0)))
                   (path t)))))

(defn scene []
  (csvg/timed
   (csvg/svg {:width width
              :height height
              :stroke "black"
              :fill "white"
              :stroke-width 1.0}
     (shapes))))

(sketch/definition all-the-shapes-in-between
  {:created-at "2023-01-02"
   :type :svg
   :tags #{:genuary2023}}
  (ctrl/mount (view-sketch/page-for scene :all-the-shapes-in-between)
              "sketch-host"))
