(ns shimmers.sketches.tree-rings
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]))

;; inspired by https://gorillasun.de/blog/radial-perlin-noise-and-generative-tree-rings

(def width 900)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn ring [r n]
  (let [points (for [t (range 0 eq/TAU (/ eq/TAU n))]
                 (g/as-cartesian (gv/vec2 r t)))]
    (csvg/path
     (concat [[:M (first points)]]
             (mapv (fn [p] [:L p]) (rest points))
             [[:Z]]))))

(defn shapes []
  (let [radius (int (/ height 2.03))
        rings 20
        points 60]
    (for [r (drop 1 (range 0 radius (/ radius rings)))]
      (ring r points))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 1.0}
    [[:g {:transform (csvg/translate (rv 0.5 0.5))}
      (shapes)]]))

(sketch/definition tree-rings
  {:created-at "2022-10-22"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :tree-rings)
              "sketch-host"))
