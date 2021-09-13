(ns shimmers.algorithm.lines-test
  (:require [cljs.test :as t :refer-macros [deftest is] :include-macros true]
            [shimmers.algorithm.lines :as sut]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.vector :as gv]))

(def points [(gv/vec2 0 0) (gv/vec2 1 2.1) (gv/vec2 2 (/ 1 3)) (gv/vec2 3 1) (gv/vec2 2 2)])
(def lines [(gl/line2 [0 0] [1 2.1])
            (gl/line2 [1 2.1] [2 (/ 1 3)])
            (gl/line2 [2 (/ 1 3)] [3 1])
            (gl/line2 [3 1] [2 2])])

(deftest points->lines
  (is (= lines (sut/points->lines points))))

(deftest lines->points
  (is (= points (sut/lines->points lines))))

(deftest segment-at
  (is (= [(gl/line2 0 0 1 1)
          (gl/line2 1 1 2 2)]
         (sut/segment-at (gl/line2 0 0 2 2))))
  (is (= [(gl/line2 0 0 0.5 0.5)
          (gl/line2 0.5 0.5 4 4)]
         (sut/segment-at (gl/line2 0 0 4 4) 0.125))))

(deftest segmented
  (is (= [(gl/line2 0 0 0 5) (gl/line2 0 5 0 10)]
         (sut/segmented (gl/line2 0 0 0 10) 2)))
  (is (= [(gl/line2 0 0 1 3) (gl/line2 1 3 2 6) (gl/line2 2 6 3 9)]
         (sut/segmented (gl/line2 0 0 3 9) 3)))
  (is (thrown-with-msg? js/Error #"pos-int"
                        (sut/segmented (gl/line2 0 0 1 0) 1.1))))

(comment (t/run-tests))
