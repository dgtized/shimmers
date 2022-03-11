(ns shimmers.algorithm.delaunay
  (:require d3-delaunay
            [thi.ng.geom.polygon :as gp]))

(set! *warn-on-infer* true)

(defn delaunay-triangles [points]
  (let [delaunay (js/d3.Delaunay.from (clj->js points))]
    (for [poly (.trianglePolygons delaunay)]
      (gp/polygon2 (js->clj poly)))))

(comment (delaunay-triangles [[0 0] [0 10] [10 10] [0 10] [3 3] [7 7] [3 7]]))

