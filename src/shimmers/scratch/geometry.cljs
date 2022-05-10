(ns shimmers.scratch.geometry
  (:require
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; FIXME: inlined from geom.utils, to explore problems with centroid of concave
;; polygons
(defn fit-all-into-bounds
  "Takes an AABB or rect and seq of shapes, proportionally scales and
  repositions all items to fit into given bounds. Returns lazyseq of
  transformed entities. Does not support collections of mixed 2D/3D
  entities. Use rects as target bounds for 2D colls."
  [bounds coll]
  ;; check minimum bounds
  (let [b (update (gu/coll-bounds coll) :size tm/max (gv/vec2 1 1))
        s (reduce min (tm/div (get bounds :size) (get b :size)))
        b' (g/center (g/scale b s) (g/centroid bounds))
        concave (some poly-detect/concave? coll)]
    (for [shape coll
          ;; temporary hardcoded offset
          :let [center (if (and concave (> (count (:points shape)) 2))
                         (tm/+ (g/centroid shape) (gv/vec2 0 2))
                         (g/centroid shape))]]
      (-> shape
          (g/center (g/unmap-point b' (g/map-point b center)))
          (g/scale-size s)))))

(comment
  (let [horizontal (gl/line2 [0 5] [10 5])
        vertical (gl/line2 [5 0] [5 10])
        diagonal (gl/line2 [0 0] [10 10])
        bounds (rect/rect 0 0 8 8)]
    {:bounds-h (gu/coll-bounds [horizontal])
     :bounds-v (gu/coll-bounds [vertical])
     :bounds-d (gu/coll-bounds [diagonal])
     :fit-h (fit-all-into-bounds bounds [horizontal])
     :fit-v (fit-all-into-bounds bounds [vertical])
     :fit-d (fit-all-into-bounds bounds [diagonal])}))
