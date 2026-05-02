(ns shimmers.math.geometry.collisions-test
  (:require
   [clojure.test :as t :refer [deftest is] :include-macros true]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.collisions :as sut]
   [shimmers.math.geometry.line]
   [shimmers.math.geometry.points :as mgp]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(deftest overlap?
  (t/testing "Rect2 Triangle2"
    (is (sut/overlaps? (rect/rect 2) (gt/triangle2 [0.0 1.0] [1.5 1.5] [1.0 0.5])))
    (is (sut/overlaps? (rect/rect 2) (gt/triangle2 [0.5 1.0] [2.5 1.5] [1.0 0.5])))
    (is (not (sut/overlaps? (rect/rect 1) (gt/triangle2 [2.0 2.0] [1.5 1.25] [3.0 2.0])))))
  (t/testing "Rect2 Rect2"
    (is (sut/overlaps? (rect/rect 4) (rect/rect 4)) "identity")
    (is (sut/overlaps? (rect/rect 3) (rect/rect 2)))
    (is (sut/overlaps? (rect/rect 4) (rect/rect 1 1 2)) "contains")
    (is (sut/overlaps? (rect/rect 4) (rect/rect 2 2 4)) "overlap")
    (is (sut/overlaps? (rect/rect 4) (rect/rect 0 4 2 2)) "touching edge"))
  (t/testing "Rect2 Circle2"
    (is (sut/overlaps? (rect/rect 4) (gc/circle 2)) "overlap")
    (is (sut/overlaps? (rect/rect 4) (gc/circle 2 2 1)) "contains")
    (is (not (sut/overlaps? (rect/rect 2) (gc/circle [3.0 3.0] 1.0))) "outside"))
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
    (is (sut/overlaps? (rect/rect 4) (gl/line2 [-3 2] [5 3])) "line crosses rectangle"))
  (t/testing "Circle2 Triangle2"
    (is (sut/overlaps? (gc/circle [1 1] 1) (gt/triangle2 [1 1.5] [3.0 2.0] [1.5 3.0])))
    (is (sut/overlaps? (gc/circle [1 1] 1) (gt/triangle2 [0.5 0.5] [1.5 1.25] [0.75 1.0])))
    (is (not (sut/overlaps? (gc/circle [1 1] 1) (gt/triangle2 [5 4] [2 3] [3 7])))))
  (t/testing "Circle2 Polygon2"
    (is (sut/overlaps? (gc/circle 10) (gp/polygon2 [2 0] [2 4] [12 5] [3 1])))
    (is (sut/overlaps? (gc/circle 10) (gp/polygon2 [2 0] [2 4] [5 5] [1 1] [2 1])))
    (is (not (sut/overlaps? (gc/circle 1) (gp/polygon2 [6 2] [3 4] [5 5] [3 4] [2 3]))))))

(deftest bounded
  (t/testing "Triangle2 Vec2"
    (is (sut/bounded? (gt/triangle2 [0 0] [1 1] [1 0])
                      (gv/vec2 0.5 0.5)))
    (is (not (sut/bounded? (gt/triangle2 [0 0] [1 1] [1 0])
                           (gv/vec2 2.0 0.0)))))
  (t/testing "Triangle2 Line2"
    (is (sut/bounded? (gt/triangle2 [0 0] [1 1] [1 0])
                      (gl/line2 0.4 0.25 0.6 0.25)))
    (is (not (sut/bounded? (gt/triangle2 [0 0] [1 1] [1 0])
                           (gl/line2 0.4 0.25 1.6 0.25)))))
  (t/testing "Triangle2 Triangle2"
    (is (sut/bounded? (gt/triangle2 [0 0] [1 1] [1 0])
                      (gt/triangle2 [0 0] [1 1] [1 0]))
        "identity")
    (is (sut/bounded? (gt/triangle2 [0 0] [0 1] [1 0])
                      (gt/triangle2 [0.1 0.1] [0.1 0.9] [0.9 0.1]))
        "contained")
    (is (not (sut/bounded? (gt/triangle2 [0 0] [0 1] [1 0])
                           (gt/triangle2 [0 0] [1 1] [1 0])))
        "crossing outside")
    (is (not (sut/bounded? (gt/triangle2 [0 0] [0 1] [1 0])
                           (gt/triangle2 [2 0] [2 1] [3 0])))
        "outside"))
  (t/testing "Triangle2 Rect2"
    (is (sut/bounded? (gt/triangle2 [0 0] [0 2] [2 0])
                      (rect/rect 0 0 1 1)))
    (is (not (sut/bounded? (gt/triangle2 [0 0] [0 2] [2 0])
                           (rect/rect 1 0 1 1)))
        "overlap")
    (is (not (sut/bounded? (gt/triangle2 [0 0] [0 2] [2 0])
                           (rect/rect 5 0 1 1)))
        "outside"))
  (t/testing "Triangle2 Polygon2"
    (is (sut/bounded? (gt/triangle2 [0 0] [0 2] [2 0])
                      (g/as-polygon (gc/circle 0.5 0.5 0.5) 6)))
    (is (not (sut/bounded? (gt/triangle2 [0 0] [0 2] [2 0])
                           (g/as-polygon (gc/circle 1.0 0.5 0.5) 6)))))
  (t/testing "Triangle2 Circle2"
    (is (sut/bounded? (gt/triangle2 [0 0] [0 2] [2 0])
                      (gc/circle 0.5 0.5 0.2)))
    (is (not (sut/bounded? (gt/triangle2 [0 0] [0 2] [2 0])
                           (gc/circle 0.5 0.5 0.6))))
    (is (not (sut/bounded? (gt/triangle2 [0 0] [0 2] [2 0])
                           (gc/circle 3 1.0 0.6)))))
  (t/testing "Rect2 Vec2"
    (is (sut/bounded? (rect/rect 2) (gv/vec2 0.5 0.5)))
    (is (sut/bounded? (rect/rect 2) (gv/vec2)))
    (is (not (sut/bounded? (rect/rect 2) (gv/vec2 -1 0)))))
  (t/testing "Rect2 Line2"
    (is (sut/bounded? (rect/rect 2) (gl/line2 0.5 0.5 1.5 1.5)))
    (is (not (sut/bounded? (rect/rect 2) (gl/line2 0.5 0.5 3.0 3.0))))
    (is (not (sut/bounded? (rect/rect 2) (gl/line2 3.0 2.0 4.0 1.0)))))
  (t/testing "Rect2 Triangle2"
    (is (sut/bounded? (rect/rect 2) (gt/triangle2 [0.5 0.5] [0.5 1.5] [1.5 0.5])))
    (is (not (sut/bounded? (rect/rect 2) (gt/triangle2 [0.5 0.5] [3.0 1.5] [1.5 0.5])))))
  (t/testing "Rect2 Rect2"
    (is (sut/bounded? (rect/rect 2) (rect/rect 1)))
    (is (not (sut/bounded? (rect/rect 1) (rect/rect 2))))
    (is (not (sut/bounded? (rect/rect 1) (g/translate (rect/rect 1) (gv/vec2 1.0 0.0))))))
  (t/testing "Rect2 Polygon2"
    (is (sut/bounded? (rect/rect 2) (g/as-polygon (gc/circle 1.0 1.0 1.0) 5)))
    (is (not (sut/bounded? (rect/rect 2) (g/as-polygon (gc/circle 1.5 1.0 1.0) 5)))))
  (t/testing "Rect2 Circle2"
    (is (sut/bounded? (rect/rect 2) (gc/circle 1.0 1.0 1.0)))
    (is (not (sut/bounded? (rect/rect 2) (gc/circle 1.5 1.0 1.0)))))
  (t/testing "Polygon2 Triangle2"
    (is (sut/bounded? (gp/polygon2 [0 0] [0 1] [1 1] [1 0.5])
                      (gt/triangle2 [0.1 0.1] [0.1 0.9] [0.9 0.9])))
    (is (sut/bounded? (gp/polygon2 [0 0] [0 2] [2 0])
                      (gt/triangle2 [0.5 0.5] [0.5 1.5] [1.5 0.5])))
    (is (sut/bounded? (gp/polygon2 [0 0] [0 1] [1 1] [1 0.5])
                      (gt/triangle2 [0 0] [0 1] [1 1])))
    (is (not (sut/bounded? (gp/polygon2 [0 0] [0 1] [1 1] [1 0.5])
                           (gt/triangle2 [0 0] [0 1] [2 1])))))
  (t/testing "Polygon2 Rect2"
    (is (sut/bounded? (gp/polygon2 [0.0 0.0] [0.0 2.0] [2.0 2.0] [2.0 0.0])
                      (rect/rect 2)))
    (is (not (sut/bounded? (gp/polygon2 [0.0 0.0] [0.5 1.0] [0.0 2.0] [2.0 2.0] [2.0 0.0])
                           (rect/rect 2)))))
  (t/testing "Polygon2 Circle2"
    (is (sut/bounded? (gp/polygon2 [0.0 0.0] [0.5 1.0] [0.0 2.0] [2.0 2.0] [2.0 0.0])
                      (gc/circle 1 1 0.5)))
    (is (sut/bounded? (gp/polygon2 [0 0] [0 1] [0.98 1] [1 0.98] [1 0])
                      (gc/circle 0.5 0.5 0.5)))
    (is (not (sut/bounded? (gp/polygon2 [0.0 0.0] [0.5 1.0] [0.0 2.0] [2.0 2.0] [2.0 0.0])
                           (gc/circle 1 1 1)))))
  (t/testing "Circle2 Vec2"
    (is (sut/bounded? (gc/circle 2) (gv/vec2)) "inside")
    (is (sut/bounded? (gc/circle 2) (gv/vec2 2 0)) "on radius")
    (is (not (sut/bounded? (gc/circle 2) (gv/vec2 4 4)))))
  (t/testing "Circle2 Line2"
    (is (sut/bounded? (gc/circle 2) (gl/line2 0 0 1 1)))
    (is (sut/bounded? (gc/circle 2) (gl/line2 0 0 2 0)))
    (is (not (sut/bounded? (gc/circle 2) (gl/line2 1 0 3 1)))))
  (t/testing "Circle2 Triangle2"
    (is (sut/bounded? (gc/circle 1) (gt/triangle2 [0 0] [1 0] [0 1])))
    (is (not (sut/bounded? (gc/circle 1) (gt/triangle2 [0 0] [2 0] [0 1])))))
  (t/testing "Circle2 Rect2"
    (is (sut/bounded? (gc/circle 2) (rect/rect 1)))
    (is (not (sut/bounded? (gc/circle 2) (rect/rect 2)))))
  (t/testing "Circle2 Polygon2"
    (is (sut/bounded? (gc/circle 2) (g/as-polygon (gc/circle 1) 5)))
    (is (not (sut/bounded? (gc/circle 2) (g/as-polygon (gc/circle 1.25 0 1) 6)))))
  (t/testing "Circle2 Circle2"
    (is (sut/bounded? (gc/circle 2) (gc/circle 1)))
    (is (sut/bounded? (gc/circle 2) (gc/circle 2)))
    (is (not (sut/bounded? (gc/circle 2) (gc/circle 3))))
    (is (sut/bounded? (gc/circle 2) (gc/circle [1 0] 1)))
    (is (not (sut/bounded? (gc/circle 2) (gc/circle [1.1 0] 1))))
    (is (not (sut/bounded? (gc/circle 2) (gc/circle [1 0] 2))))))

(deftest coincident-segment?
  (is (tm/delta= (sut/coincident-segment? [(gv/vec2 0 0) (gv/vec2 0 5)]
                                          [(gv/vec2 0 1) (gv/vec2 0 6)])
                 (gl/line2 [0 1] [0 5]))
      "coincident overlapping edge")
  (is (tm/delta= (sut/coincident-segment? [(gv/vec2 0 0) (gv/vec2 0 5)]
                                          [(gv/vec2 0 5) (gv/vec2 0 6)])
                 (gl/line2 [0 5] [0 5]))
      "coincident edge with one connecting vertice")
  (is (nil? (sut/coincident-segment? [(gv/vec2 0 0) (gv/vec2 0 5)]
                                     [(gv/vec2 0 5) (gv/vec2 1 5)]))
      "touching vertice but edge is not coincident")

  (is (nil? (sut/coincident-segment? [(gv/vec2 0 0) (gv/vec2 0 4)]
                                     [(gv/vec2 0 5) (gv/vec2 0 6)]))
      "same line in plane, but with no overlap"))

(deftest coincident-edge?
  (t/testing "Rect2 Rect2"
    (is (sut/coincident-edge? (rect/rect 4) (rect/rect 2)) "inside touching edge")
    (is (sut/coincident-edge? (rect/rect 4) (rect/rect 0 4 2 2)) "outside touching edge")
    (is (sut/coincident-edge? (rect/rect 4) (rect/rect 4 0 2 2)) "outside touching edge")
    (is (not (sut/coincident-edge? (rect/rect 4) (rect/rect 4 4 2 2))) "outside touching point")
    (is (sut/coincident-edge? (rect/rect 4) (rect/rect 0 3 2 2)) "overlapping edge")

    (let [xs (g/subdivide (rect/rect 3 3) {:rows 3 :cols 3})
          t (rect/rect 1 1 1 1)]
      (is (= 5 (count (filter (fn [s] (sut/coincident-edge? t s)) xs)))))))

(deftest coincident-point?
  (t/testing "Polygon2 Polygon2"
    (is (nil? (sut/coincident-point? (gp/polygon2 [0.0 1] [5 2] [2.5 3])
                                     (gp/polygon2 [0.0 0] [5 0] [2.5 -3]))))
    (is (tm/delta= (sut/coincident-point? (gp/polygon2 [0.1 0] [5 0.2] [2.5 3])
                                          (gp/polygon2 [0.0 0] [5 0.2] [2.5 -3]))
                   [5 0.2]))
    (is (tm/delta= (sut/coincident-point? (gp/polygon2 [0 0] [10 0] [5 -5])
                                          (gp/polygon2 [5 0] [10 5] [0 5]))
                   [5 0])
        "point intersects with other shapes edge")
    (is (nil? (sut/coincident-point? (gp/polygon2 [0 0] [5 -5] [10 0])
                                     (gp/polygon2 [0 1] [5 -6] [10 1])))
        "coincident point must be at the intersection point not inside or through")))

(deftest coincident-points
  (is (empty? (sut/coincident-points (gp/polygon2 [0.0 1] [5 2] [2.5 3])
                                     (gp/polygon2 [0.0 0] [5 0] [2.5 -3]))))
  (is (tm/delta= (first (sut/coincident-points (gp/polygon2 [0.1 0] [5 0.2] [2.5 3])
                                               (gp/polygon2 [0.0 0] [5 0.2] [2.5 -3])))
                 [5 0.2]))
  (is (tm/delta= (first (sut/coincident-points (gp/polygon2 [0 0] [10 0] [5 -5])
                                               (gp/polygon2 [5 0] [10 5] [0 5])))
                 [5 0])
      "point intersects with other shapes edge")
  (is (empty? (sut/coincident-points (gp/polygon2 [0 0] [5 -5] [10 0])
                                     (gp/polygon2 [0 1] [5 -6] [10 1])))
      "coincident point must be at the intersection point not inside or through")

  (is (mgp/points-delta=
       (sut/coincident-points (gp/polygon2 [0 0] [10 0] [10 10] [0 10])
                              (gp/polygon2 [10 0] [20 0] [20 10] [10 10]))
       [[10 0] [10 10]])
      "polygons share an edge")

  #_(is (= (sut/coincident-points (gp/polygon2 [0 0] [10 0] [10 10] [0 10])
                                  (gp/polygon2 [10 -1] [20 0] [20 10] [10 11]))
           [[10 0] [10 10]])
        "polygons cover a shared edge")

  #_(is (= (sut/coincident-points (gp/polygon2 [0 0] [10 0] [10 10] [0 10])
                                  (gp/polygon2 [10 0] [20 0] [20 20] [10 20]))
           [[10 0] [10 10]])
        "polygons overlap a shared edge"))

(deftest same-edge
  (let [a (gv/vec2 0 0)
        b (gv/vec2 5 0)
        c (gv/vec2 2 0)
        d (gv/vec2 6 0)]
    (is (= [a b] (sut/same-edge [a b] [a b]))
        "same")
    (is (= [a b] (sut/same-edge [a b] [b a]))
        "same reversed")
    (is (= [c b] (sut/same-edge [a b] [c d]))
        "coincident-overlap at start")
    (is (= [c b] (sut/same-edge [c d] [a b]))
        "coincident-overlap at end")
    (is (= [c b] (sut/same-edge [c b] [a d]))
        "coincident-covers")
    (is (not (sut/same-edge [a c] [b d]))
        "coincident but non-adjacent")
    (is (not (sut/same-edge [a b] [a (gv/vec2 0 5)]))
        "edges adjacent at point")
    (is (not (sut/same-edge [a b] [b d]))
        "coincident edges adjacent at point")
    (is (not (sut/same-edge [a b] [(gv/vec2 2 -1) (gv/vec2 2 5)]))
        "intersection")))
