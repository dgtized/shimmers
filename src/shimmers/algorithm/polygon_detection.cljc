(ns shimmers.algorithm.polygon-detection
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]
   [thi.ng.geom.vector :as gv]))

(defn atan2 [[x y]]
  (Math/atan2 y x))

(defn small-angle-between [a b]
  (Math/abs (- (g/heading a) (g/heading b))))

;; From a starting location with surrounding candidate nodes
;; Return the counter-clockwise candidate of the pair with the smallest angle between
;; Note: heuristic, doesn't guarentee path started from here is start of a clockwise polygon
(defn clockwise-starts [start candidates]
  (let [ordered (sort-by (fn [p] (g/heading (tm/- p start))) candidates)
        [a b]
        (->> (vec ordered)
             (cons (last ordered))
             (partition 2 1)
             (sort-by (fn [[a b]] [(small-angle-between (tm/- a start) (tm/- b start)) a b]))
             first)]
    (if (> (g/angle-between (tm/- a start) (tm/- b start))
           (g/angle-between (tm/- b start) (tm/- a start)))
      b a)))

(comment
  (g/heading (gv/vec2 -1 0))
  (g/heading (gv/vec2 -1 1))
  (g/heading (gv/vec2 0 1))
  (g/heading (gv/vec2 1 1))
  (g/heading (gv/vec2 1 0))
  (g/angle-between (gv/vec2 -1 1) (gv/vec2 0 1))
  (g/angle-between (gv/vec2 0 1) (gv/vec2 -1 1))
  (small-angle-between (gv/vec2 -1 1) (gv/vec2 0 1))
  (small-angle-between (gv/vec2 0 1) (gv/vec2 -1 1))

  (g/angle-between (gv/vec2 0 1) (gv/vec2 1 0))
  (g/angle-between (gv/vec2 1 0) (gv/vec2 0 1))
  (small-angle-between (gv/vec2 1 0) (gv/vec2 0 1))
  (small-angle-between (gv/vec2 0 1) (gv/vec2 1 0))
  )
