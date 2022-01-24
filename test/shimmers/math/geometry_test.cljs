(ns shimmers.math.geometry-test
  (:require
   [cljs.test :as t
    :refer-macros [deftest is run-tests]
    :include-macros true]
   [shimmers.math.geometry :as sut]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(deftest radial-sort
  (let [points (map gv/vec2 [[0 0]
                             [1 0] [1 1] [0 1]
                             [-1 1] [-1 0] [-1 -1]
                             [0 -1] [1 -1]])]
    (is (= [[0 0] [1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1]]
           (sut/radial-sort (gv/vec2 0 0) points)))
    (is (= [[1 1] [0 1] [-1 1] [0 0] [1 0] [-1 0] [-1 -1] [0 -1] [1 -1]]
           (sut/radial-sort (gv/vec2 2 0) points)))))

(deftest circle-circle-intersection
  (let [c1 (gc/circle 1)
        c2 (gc/circle 1)
        sqrt3_2 (/ (Math/sqrt 3) 2)]
    (is (some? (g/intersect-shape c1 c2))
        "directly overlapping is an intersection with no resolution")
    ;; Returns [(gv/vec2 (/ 0 0) (/ 0 0)) (gv/vec2 (/ 0 0) (/ 0 0))]
    (is (tm/delta= (g/intersect-shape c1 (g/translate c2 (gv/vec2 0.5 0)))
                   [(gv/vec2 0.25 0.968246) (gv/vec2 0.25 -0.968246)])
        "at direct overlap find the intersection points")
    (is (= (g/intersect-shape c1 (g/translate c2 (gv/vec2 1 0)))
           [(gv/vec2 0.5 sqrt3_2) (gv/vec2 0.5 (- sqrt3_2))])
        "at direct overlap find the intersection points")
    (is (= (g/intersect-shape c1 (g/translate c2 (gv/vec2 2 0)))
           [(gv/vec2 1 0) (gv/vec2 1 0)])
        "at furtherest overlap find the intersection points")
    (is (not (g/intersect-shape c1 (g/translate c2 (gv/vec2 2.0001 0))))
        "outside of range, return no intersection")))

(comment (run-tests))
