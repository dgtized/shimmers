(ns shimmers.sketches.delaunator
  (:require
   [delaunator]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.geom.line :as gl]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn gen-point []
  (rv (dr/random 0.05 0.95) (dr/random 0.05 0.95)))

;; https://mapbox.github.io/delaunator/
(defn next-half-edge [e]
  (if (= 2 (mod e 3)) (- e 2) (+ e 1)))

(defn prev-half-edge [e]
  (if (= 0 (mod e 3)) (+ e 2) (- e 1)))

(defn edges [points]
  (let [delaunay (js/Delaunator.from (clj->js points))]
    (for [e (.-triangles delaunay)
          :when (> e (aget (.-halfedges delaunay) e))
          :let [p (nth points (aget (.-triangles delaunay) e))
                q (nth points (aget (.-triangles delaunay) (next-half-edge e)))]]
      (gl/line2 p q))))

(comment
  (edges (repeatedly 10 gen-point)))

(defn shapes []
  (let [points (repeatedly 10 gen-point)
        triangle-edges (edges points)]
    [(svg/group {:fill "black"}
                (for [p points]
                  (gc/circle p 2)))
     (svg/group {} triangle-edges)]))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.5}
            (apply list (shapes))))

(sketch/definition delaunator
  {:created-at "2022-05-08"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :delaunator)
              "sketch-host"))
