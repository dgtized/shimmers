(ns shimmers.algorithm.lines-test
  (:require
   [cljs.test :as t :refer-macros [deftest is] :include-macros true]
   [shimmers.algorithm.lines :as sut]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
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

(deftest clip-line
  (is (= [] (sut/clip-line (gl/line2 0 0 10 10) (rect/rect 10 2 5 5)))
      "line does not intersect polygon")
  (is (= [(gl/line2 5 5 7 7)] (sut/clip-line (gl/line2 5 5 7 7) (rect/rect 1 1 9 9)))
      "line is completely inside of polygon")
  (is (= [(gl/line2 2 2 6 6)] (sut/clip-line (gl/line2 0 0 10 10) (rect/rect 1 2 5 5)))
      "clipped inside")
  (is (= [(gl/line2 5 5 6 6)] (sut/clip-line (gl/line2 5 5 10 10) (rect/rect 2 2 4 6)))
      "line segment starts inside of shape")
  (is (= [(gl/line2 2 2 5 5)] (sut/clip-line (gl/line2 0 0 5 5) (rect/rect 2 0 8 8)))
      "line segment ends inside of shape")
  (is (= [(gl/line2 2 2 5 5)] (sut/clip-line (gl/line2 0 0 5 5) (rect/rect 2 2 8 8)))
      "line segment clips a corner from outside")
  (is (= [(gl/line2 5 5 2 2)] (sut/clip-line (gl/line2 5 5 0 0) (rect/rect 2 2 8 8)))
      "line segment clips a corner from inside")
  (is (= [(gl/line2 0.5 1.5 1.5 1.5)] (sut/clip-line (gl/line2 0.5 1.5 2 1.5) (gp/polygon2 [0 0] [1.5 1.5] [0 3])))
      "line segment clips a corner from inside, floating point")
  (is (= [] (sut/clip-line (gl/line2 1.5 1.5 0 0) (gp/polygon2 [3 1.5] [1.5 1.5] [1.5 3])))
      "segment intersects vertex at start")
  (is (= [] (sut/clip-line (gl/line2 0 0 1.5 1.5) (gp/polygon2 [3 1.5] [1.5 1.5] [1.5 3])))
      "segment intersects vertex at end")
  (is (= [(gl/line2 1 1 2 1)] (sut/clip-line (gl/line2 0 1 3 1) (gp/polygon2 [1 1] [2 1] [2 2] [1 2])))
      "segment is coincident with an edge")
  (is (= [(gl/line2 2 5 3 5) (gl/line2 7 5 8 5)]
         (sut/clip-line (gl/line2 0 5 10 5) (gp/polygon2 [2 0] [8 0] [8 10] [7 10] [7 2] [3 2] [3 10] [2 10])))
      "line segment clips multiple regions of a concave polygon"))

(comment (t/run-tests))
