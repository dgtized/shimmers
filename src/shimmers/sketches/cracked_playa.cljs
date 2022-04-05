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

;; need to actually split into sub-polygons on isec hit, then can determine which to filter out.
;; this is not removing the points between the two isec points.
(defn remove-self-intersection [polygon]
  (if-let [isec (poly-detect/self-intersecting? polygon)]
    (let [vertices (g/vertices polygon)]
      (recur (-> (for [[p q] (partition 2 1 (cons (last vertices) (vec vertices)))]
                   (if (point-on-line? p q isec)
                     isec
                     q))
                 dedupe
                 gp/polygon2)))
    polygon))

;; TODO: add rough edges to each polygon?
(defn shapes []
  (let [bounds (rect/rect 0 0 width height)
        points (pds/generate-dynamic bounds 10 [18 256] noise-at-point)
        cells (delvor/voronoi-cells points bounds)]
    (for [cell cells
          :let [width (dr/random -0.5 -4)
                inset (gp/polygon2 (gp/inset-polygon (:points cell) width))]
          ;; :when (poly-detect/self-intersecting? inset)
          ]
      (remove-self-intersection inset))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.5}
            (apply list (shapes))))

(sketch/definition cracked-playa
  {:created-at "2022-04-03"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :cracked-playa)
              "sketch-host"))
