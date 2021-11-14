(ns shimmers.algorithm.lines-test
  (:require
   [cljs.test :as t :refer-macros [deftest is] :include-macros true]
   [shimmers.algorithm.lines :as sut]
   [thi.ng.geom.core :as g]
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

(deftest points-between
  (let [line (gl/line2 0 0 100 100)
        diagonal-pts (map (fn [p] (gv/vec2 (map int p))) (g/sample-uniform line 10 true))]
    (is (= [[14 14] [21 21] [28 28]]
           (sut/points-between diagonal-pts 0.1 0.3)))
    (is (= []
           (sut/points-between diagonal-pts 0.5 0.5)))
    (is (thrown-with-msg? js/Error #"<= t0 t1"
                          (sut/points-between diagonal-pts 1.0 0.0)))
    (is (= [[0 0] [7 7]]
           (sut/points-between diagonal-pts 0.0 0.1)))
    (is (= [[91 91] [98 98] [100 100]]
           (sut/points-between diagonal-pts 0.9 1.0)))))

(comment (t/run-tests))
