(ns shimmers.sketches.cracked-playa
  (:require
   [shimmers.algorithm.delaunay :as delvor]
   [shimmers.algorithm.poisson-disc-sampling :as pds]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [thi.ng.math.noise :as noise]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn noise-at-point [p]
  (let [[x y] (tm/* p 0.01)]
    (tm/clamp01 (noise/noise2 x y))))

(defn point-on-line?
  [p q point]
  (when (tm/delta= (+ (g/dist p point) (g/dist point q))
                   (g/dist p q))
    point))

;; Not quite working, looks like it leaves an extra triangle not part of a or b,
;; or only fills out a or b but not both? Leaving it in debug view for now
(defn split-self-intersection [polygon]
  (if-let [isec (poly-detect/self-intersecting? polygon)]
    (loop [[[p q] & edges] (g/edges polygon)
           a [] b [] in-split-polygon false]
      (cond (empty? edges)
            [(with-meta polygon {:stroke-width 1.0})
             (with-meta (gp/polygon2 a) {:fill-opacity 0.1 :fill "green"})
             (with-meta (gp/polygon2 b) {:fill-opacity 0.1 :fill "red"})
             (gc/circle isec 1.0)]
            (point-on-line? p q isec)
            (if-not in-split-polygon
              (recur edges
                     (conj a p)
                     (conj b isec)
                     true)
              (recur edges
                     (conj a isec)
                     (conj b p)
                     false))
            :else
            (if in-split-polygon
              (recur edges
                     a
                     (conj b p)
                     true)
              (recur edges
                     (conj a p)
                     b
                     false))))
    [polygon]))

(comment
  (let [overlap (gp/polygon2 [(gv/vec2 0 0) (gv/vec2 10 0) (gv/vec2 0 10) (gv/vec2 10 10)])]
    [(poly-detect/self-intersecting? overlap)
     (map first (g/edges overlap))
     (split-self-intersection overlap)]))

;; TODO: add rough edges to each polygon?
(defn shapes []
  (let [bounds (rect/rect 0 0 width height)
        points (pds/generate-dynamic bounds 10 [18 256] noise-at-point)
        cells (delvor/voronoi-cells points bounds)]
    (apply concat
           (for [cell cells
                 :let [width (dr/random -0.5 -4)
                       inset (gp/polygon2 (gp/inset-polygon (:points cell) width))]
                 :when (poly-detect/self-intersecting? inset)
                 ]
             (split-self-intersection inset)))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 0.5}
            (apply list (shapes))))

(sketch/definition cracked-playa
  {:created-at "2022-04-03"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :cracked-playa)
              "sketch-host"))
