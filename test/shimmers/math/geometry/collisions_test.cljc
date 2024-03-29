(ns shimmers.math.geometry.collisions-test
  (:require
   [clojure.test :as t :refer [deftest is] :include-macros true]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.collisions :as sut]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]))

(deftest overlap?
  (t/testing "Rect2 Rect2"
    (is (sut/overlaps? (rect/rect 4) (rect/rect 4)) "identity")
    (is (sut/overlaps? (rect/rect 4) (rect/rect 1 1 2)) "contains")
    (is (sut/overlaps? (rect/rect 4) (rect/rect 2 2 4)) "overlap")
    (is (sut/overlaps? (rect/rect 4) (rect/rect 0 4 2 2)) "touching edge"))
  (t/testing "Rect2 Circle2"
    (is (sut/overlaps? (rect/rect 4) (gc/circle 2)) "overlap")
    (is (sut/overlaps? (rect/rect 4) (gc/circle 2 2 1)) "contains"))
  (t/testing "Polygon2 Circle2"
    (is (sut/overlaps? (g/rotate (g/center (rect/rect 4)) (* 0.25 eq/TAU))
                       (gc/circle 1)) "polygon contains")
    (is (sut/overlaps? (g/rotate (g/center (rect/rect 1)) (* 0.25 eq/TAU))
                       (gc/circle 4)) "circle contains"))
  (t/testing "Rect2 Line2"
    (is (sut/overlaps? (rect/rect 4) (gl/line2 [4 4] [5 4])) "line starts at vertex")
    (is (sut/overlaps? (rect/rect 4) (gl/line2 [3 5] [5 3])) "line crosses corner")
    (is (sut/overlaps? (rect/rect 4) (gl/line2 [4 3] [4 2])) "line overlaps edge")
    (is (sut/overlaps? (rect/rect 4) (gl/line2 [2 2] [3 2])) "line inside rectangle")
    (is (sut/overlaps? (rect/rect 4) (gl/line2 [2 2] [5 3])) "line exits rectangle")
    (is (sut/overlaps? (rect/rect 4) (gl/line2 [-3 2] [5 3])) "line crosses rectangle")))

(deftest bounded
  (is (sut/bounded? (gc/circle 2) (gv/vec2)) "inside")
  (is (sut/bounded? (gc/circle 2) (gv/vec2 2 0)) "on radius")
  (is (not (sut/bounded? (gc/circle 2) (gv/vec2 4 4))))
  (is (sut/bounded? (gc/circle 2) (gc/circle 1)))
  (is (sut/bounded? (gc/circle 2) (gc/circle 2)))
  (is (not (sut/bounded? (gc/circle 2) (gc/circle 3))))
  (is (sut/bounded? (gc/circle 2) (gc/circle [1 0] 1)))
  (is (not (sut/bounded? (gc/circle 2) (gc/circle [1.1 0] 1))))
  (is (not (sut/bounded? (gc/circle 2) (gc/circle [1 0] 2)))))

(deftest coincident-edge?
  (t/testing "Rect2 Rect2"
    (is (sut/coincident-edge? (rect/rect 4) (rect/rect 0 4 2 2)) "touching edge")
    (is (not (sut/coincident-edge? (rect/rect 4) (rect/rect 0 3 2 2))) "overlapping edge")))
