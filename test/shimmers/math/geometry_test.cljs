(ns shimmers.math.geometry-test
  (:require [cljs.test :as t :include-macros true
             :refer-macros [deftest is run-tests]]
            [shimmers.math.geometry :as sut]
            [thi.ng.geom.vector :as gv]))

(deftest radial-sort
  (let [points (map gv/vec2 [[0 0]
                             [1 0] [1 1] [0 1]
                             [-1 1] [-1 0] [-1 -1]
                             [0 -1] [1 -1]])]
    (is (= [[0 0] [1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1]]
           (sut/radial-sort (gv/vec2 0 0) points)))
    (is (= [[1 1] [0 1] [-1 1] [0 0] [1 0] [-1 0] [-1 -1] [0 -1] [1 -1]]
           (sut/radial-sort (gv/vec2 2 0) points)))))

(comment (run-tests))
