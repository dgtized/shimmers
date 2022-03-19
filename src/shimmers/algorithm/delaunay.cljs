(ns shimmers.algorithm.delaunay
  (:require ["d3-delaunay"]
            [thi.ng.geom.polygon :as gp]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.vector :as gv]))

(set! *warn-on-infer* true)

;; d3-delaunay polygons and triangles API results include the initial point as
;; the closing point, so they need to be trimmed of the last point.

(defn delaunay-from ^js/Delaunay [points]
  (js/d3.Delaunay.from (clj->js points)))

(defn delaunay-triangles [points]
  (let [^js/Delaunay delaunay (delaunay-from points)]
    (for [triangle (.trianglePolygons delaunay)
          :let [[a b c] (js->clj triangle)]]
      (gt/triangle2 a b c))))

(comment (delaunay-triangles [[0 0] [0 10] [10 10] [0 10] [3 3] [7 7] [3 7]]))

(defn convex-hull [points]
  (let [^js/Delaunay delaunay (delaunay-from points)
        hull (js->clj (.hullPolygon delaunay))]
    (gp/polygon2 (butlast hull))))

(comment (convex-hull [[0 0] [5 0] [5 10]]))

(defn bounds->ranges [{[xmin ymin] :p [w h] :size}]
  [xmin ymin (+ xmin w) (+ ymin h)])

(comment (bounds->ranges (rect/rect 3 4 6 8)))

(defn voronoi-cells [points bounds]
  (let [^js/Delaunay delaunay (delaunay-from points)
        ^js/Voronoi voronoi (.voronoi delaunay (clj->js (bounds->ranges bounds)))]
    (for [cell (.cellPolygons voronoi)]
      (gp/polygon2 (butlast (js->clj cell))))))

(comment (voronoi-cells [[5 5] [8 8] [3 6] [7 4]] (rect/rect 0 0 10 10)))

(defn voronoi-circumcenters [points bounds]
  (let [^js/Delaunay delaunay (delaunay-from points)
        ^js/Voronoi voronoi (.voronoi delaunay (clj->js (bounds->ranges bounds)))]
    (for [t (range 0 (alength (.-circumcenters voronoi)) 2)]
      (gv/vec2 (aget (.-circumcenters voronoi) t)
               (aget (.-circumcenters voronoi) (inc t))))))

(comment (voronoi-circumcenters [[5 5] [8 8] [3 6] [7 4]] (rect/rect 0 0 10 10)))
