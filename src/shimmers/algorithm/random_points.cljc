(ns shimmers.algorithm.random-points
  (:require
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]))

(defn random-points
  "Generate `n` random points within a given `bounds`."
  [bounds n]
  (repeatedly n #(g/unmap-point bounds (gv/vec2 (dr/random) (dr/random)))))

;; Generates *close* to n points
(defn random-cells
  "Subdivide into ~`n` cells and pick a random point in each cell."
  [bounds n]
  (let [cells (g/subdivide bounds {:num (Math/ceil (Math/sqrt n))})]
    (for [cell cells]
      (g/unmap-point cell (gv/vec2 (dr/random) (dr/random))))))

;; Generates *close* to n points
(defn random-cell-jitter
  "Subdivide into ~`n` cells and then create a circle just touching the inside of
  the cell and pick a random point inside that circle."
  [bounds n]
  (let [cells (g/subdivide bounds {:num (Math/ceil (Math/sqrt n))})]
    (for [{[w h] :size :as cell} cells]
      (geometry/random-point-in-circle (gc/circle (g/centroid cell) (/ (min w h) 2))
                                       dr/random))))
