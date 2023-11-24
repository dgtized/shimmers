(ns shimmers.algorithm.lines-test
  (:require
   [clojure.test :as t :refer [deftest is] :include-macros true]
   [shimmers.math.geometry.line]
   [shimmers.math.geometry.polygon]
   [shimmers.algorithm.lines :as sut]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def points [(gv/vec2 0 0) (gv/vec2 1 2.1) (gv/vec2 2 (/ 1 3)) (gv/vec2 3 1) (gv/vec2 2 2)])
(def lines [(gl/line2 [0 0] [1 2.1])
            (gl/line2 [1 2.1] [2 (/ 1 3)])
            (gl/line2 [2 (/ 1 3)] [3 1])
            (gl/line2 [3 1] [2 2])])

(deftest points->lines
  (is (tm/delta= lines (sut/points->lines points))))

(deftest lines->points
  (is (tm/delta= points (sut/lines->points lines))))

(deftest segment-at
  (is (tm/delta= [(gl/line2 0 0 1 1)
                  (gl/line2 1 1 2 2)]
                 (sut/segment-at (gl/line2 0 0 2 2))))
  (is (tm/delta= [(gl/line2 0 0 0.5 0.5)
                  (gl/line2 0.5 0.5 4 4)]
                 (sut/segment-at (gl/line2 0 0 4 4) 0.125))))

(deftest segmented
  (is (tm/delta= [(gl/line2 0 0 0 5) (gl/line2 0 5 0 10)]
                 (sut/segmented (gl/line2 0 0 0 10) 2)))
  (is (tm/delta= [(gl/line2 0 0 1 3) (gl/line2 1 3 2 6) (gl/line2 2 6 3 9)]
                 (sut/segmented (gl/line2 0 0 3 9) 3)))
  #?(:cljs
     (is (thrown-with-msg? js/Error #"pos-int"
                           (sut/segmented (gl/line2 0 0 1 0) 1.1)))))

(deftest points-between
  (let [line (gl/line2 0 0 100 100)
        diagonal-pts (map (fn [p] (gv/vec2 (map int p))) (g/sample-uniform line 10 true))]
    (is (tm/delta=
         [[14 14] [21 21] [28 28]]
         (sut/points-between diagonal-pts 0.1 0.3)))
    (is (= []
           (sut/points-between diagonal-pts 0.5 0.5)))
    #?(:cljs
       (is (thrown-with-msg? js/Error #"<= t0 t1"
                             (sut/points-between diagonal-pts 1.0 0.0))))
    (is (tm/delta=
         [[0 0] [7 7]]
         (sut/points-between diagonal-pts 0.0 0.1)))
    (is (tm/delta=
         [[91 91] [98 98] [100 100]]
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
  (is (= [(gl/line2 0.5 1.5 1.5 1.5)]
         (sut/clip-line (gl/line2 0.5 1.5 2 1.5) (gp/polygon2 [0 0] [1.5 1.5] [0 3])))
      "line segment clips a corner from inside, floating point")
  (is (= [] (sut/clip-line (gl/line2 1.5 1.5 0 0) (gp/polygon2 [3 1.5] [1.5 1.5] [1.5 3])))
      "segment intersects vertex at start")
  (is (= [] (sut/clip-line (gl/line2 0 0 1.5 1.5) (gp/polygon2 [3 1.5] [1.5 1.5] [1.5 3])))
      "segment intersects vertex at end")
  (is (= [(gl/line2 1 1 2 1)]
         (sut/clip-line (gl/line2 0 1 3 1) (gp/polygon2 [1 1] [2 1] [2 2] [1 2])))
      "segment is coincident with an edge")
  (t/testing "concave polygon"
    ;;    2 3 7 8
    ;; 0  a-----b
    ;; 2  | f-e |
    ;;    | | | |
    ;; 10 h-g d-c
    (let [concave-poly (gp/polygon2 [2 0] [8 0] [8 10] [7 10] [7 2] [3 2] [3 10] [2 10])]
      (is (= [(gl/line2 2 5 3 5) (gl/line2 7 5 8 5)]
             (sut/clip-line (gl/line2 0 5 10 5) concave-poly))
          "line segment clips multiple regions of a concave polygon")
      (is (= [(gl/line2 2 5 3 5)]
             (sut/clip-line (gl/line2 0 5 7 5) concave-poly))
          "clips left tail")
      (is (= [(gl/line2 7 5 8 5)]
             (sut/clip-line (gl/line2 3 5 10 5) concave-poly))
          "clips right tail")
      (is (= [(gl/line2 2 2 3 2) (gl/line2 7 2 8 2)]
             (sut/clip-line (gl/line2 0 2 10 2) concave-poly))
          "line segment clips multiple regions of a concave polygon including coincident edges")
      (is (= [(gl/line2 2.5 2 3 2) (gl/line2 7 2 8 2)]
             (sut/clip-line (gl/line2 2.5 2 8 2) concave-poly))
          "line segment clips coincident edge of concave polygon starting inside")
      (is (= [(gl/line2 2 2 3 2) (gl/line2 7 2 7.5 2)]
             (sut/clip-line (gl/line2 2 2 7.5 2) concave-poly))
          "line segment clips coincident edge of concave polygon ending inside"))

    (is (= [(gl/line2 190 515 190 450)]
           (sut/clip-line
            (gl/line2 [190 10440] [190 -9560])
            (gp/polygon2 [155 515] [155 450]
                         [190 450] [190 430]
                         [630 430] [630 515])))
        "corner cut, but checking on coincident edge")))

(deftest remove-coincident-segments
  (is (= (gp/polygon2 [0 0] [10 0])
         (sut/remove-coincident-segments (gp/polygon2 [0 0] [10 0])))
      "line degenerate case")
  (is (= (gp/polygon2 [0 0] [10 0] [10 10])
         (sut/remove-coincident-segments (gp/polygon2 [0 0] [10 0] [10 10])))
      "triangle degenerate case")
  #_(is (= (gp/polygon2 [0 0] [10 0])
           (sut/remove-coincident-segments (gp/polygon2 [0 0] [5 0] [10 0])))
        "linestrip degenerate case")
  (is (= (gp/polygon2 [0 0] [10 0] [10 10] [0 10])
         (sut/remove-coincident-segments (gp/polygon2 [0 0] [10 0] [10 10] [0 10])))
      "rectangle with no coincident segments")
  (is (= (gp/polygon2 [0 0] [10 0] [10 10] [0 10])
         (sut/remove-coincident-segments (gp/polygon2 [0 0] [10 0] [10 10] [5 10] [0 10])))
      "rectangle with one coincident segment")
  (is (= (gp/polygon2 [10 0] [10 10] [0 10] [0 0])
         (sut/remove-coincident-segments (gp/polygon2 [1 0] [10 0] [10 10] [0 10] [0 0])))
      "coincident vertex at loop-back point")
  (is (= (gp/polygon2 [5 5] [5 0] [0 0])
         (sut/remove-coincident-segments (gp/polygon2 [1 1] [2 2] [3 3] [5 5] [5 4] [5 3] [5 0] [4 0] [0 0])))
      "coincident vertices on multiple edges and loop-back vertex"))

(defn isecs= [expected actual]
  (->> (map (fn [e a]
              (and (tm/delta= (:edge e) (:edge a))
                   (tm/delta= (:p e) (:p a))
                   (tm/delta= (:pair e) (:pair a))))
            expected actual)
       (every? true?)))

(deftest find-paired-intersections-for-cut
  (let [triangle (gp/polygon2 [0 0] [10 0] [0 10])]
    (is (empty? (sut/find-paired-intersections triangle
                                               (gl/line2 [-5 0] [5 -5])))
        "no intersections")
    (is (empty? (sut/find-paired-intersections triangle
                                               (gl/line2 [-5 5] [5 -5])))
        "glancing intersection at one vertex")
    (is (isecs=
         [{:edge [[0 0] [10 0]] :p [0 0] :pair [5 5]}
          {:edge [[10 0] [0 10]] :p [5 5] :pair [0 0]}]
         (sut/find-paired-intersections triangle (gl/line2 [0 0] [10 10])))
        "intersection with vertex & edge"))
  (let [square (rect/rect [0 0] [10 10])]
    (is (isecs=
         [{:edge [[0 0] [10 0]] :p [0 0] :pair [10 10]}
          {:edge [[10 10] [0 10]] :p [10 10] :pair [0 0]}]
         (sut/find-paired-intersections square (gl/line2 [0 0] [10 10])))
        "intersection at two vertices")
    (is (isecs=
         [{:edge [[0 10] [0 0]] :p [0 5] :pair [10 5]}
          {:edge [[10 0] [10 10]] :p [10 5] :pair [0 5]}]
         (sut/find-paired-intersections square (gl/line2 [0 5] [10 5])))
        "intersection at two edges"))
  (let [convex-poly (gp/polygon2 [0 0] [10 0] [10 10] [8 10] [8 4] [2 4] [2 10] [0 10])]
    (is (isecs=
         [{:edge [[0 10] [0 0]] :p [0 4] :pair [2 4]}
          {:edge [[2 4] [2 10]] :p [2 4] :pair [0 4]}]
         (sut/find-paired-intersections convex-poly (gl/line2 [0 4] [2 4])))
        "clip left tail")
    (is (isecs=
         [{:edge [[0 10] [0 0]] :p [0 4] :pair [2 4]}
          {:edge [[2 4] [2 10]] :p [2 4] :pair [0 4]}]
         (sut/find-paired-intersections convex-poly (gl/line2 [0 4] [8 4])))
        "clip left tail with coincident segment removed")
    (is (isecs=
         [{:edge [[8 4] [2 4]] :p [8 4] :pair [10 4]}
          {:edge [[10 0] [10 10]] :p [10 4] :pair [8 4]}]
         (sut/find-paired-intersections convex-poly (gl/line2 [8 4] [10 4])))
        "clip right tail")
    #_(is (= [{:edge [[8 10] [8 4]] :p [8 4] :pair [10 4]}
              {:edge [[10 0] [10 10]] :p [10 4] :pair [8 4]}]
             (sut/find-paired-intersections convex-poly (gl/line2 [2 4] [10 4])))
          "clip right tail with coincident segment removed")
    (is (isecs=
         [{:edge [[0 10] [0 0]] :p [0 4] :pair [2 4]}
          {:edge [[2 4] [2 10]] :p [2 4] :pair [0 4]}
          {:edge [[8 4] [2 4]] :p [8 4] :pair [10 4]}
          {:edge [[10 0] [10 10]] :p [10 4] :pair [8 4]}]
         (sut/find-paired-intersections convex-poly (gl/line2 [0 4] [10 4])))
        "coincident in middle")))

(defn roughly-same-polygon [a b]
  (let [as (g/vertices a)
        bs (g/vertices b)]
    (and (= (count as) (count bs))
         (every? (fn [[p q]] (tm/delta= p q))
                 (map vector as bs)))))

(deftest cut-polygon
  (let [poly (gp/polygon2 [0 0] [10 0] [0 10])
        [a b c] (g/vertices poly)]
    (t/testing "triangles"
      (is (= [poly] (sut/cut-polygon poly (gl/line2 [0 11] [1 11])))
          "identity if line does not intersect")
      (is (= [poly] (sut/cut-polygon poly (gl/line2 [0 10] [1 10])))
          "identity if line only intersects one corner")
      (is (= [(gp/polygon2 a [5 5] c)
              (gp/polygon2 a b [5 5])]
             (sut/cut-polygon poly (gl/line2 [0 0] [10 10])))
          "diagonal cut")
      (is (= [(gp/polygon2 a b [5 5] [0 5])
              (gp/polygon2 [5 5] c [0 5])]
             (sut/cut-polygon poly (gl/line2 [0 5] [10 5])))
          "horizontal cut")
      (is (= [(gp/polygon2 a [5 0] [5 5] c)
              (gp/polygon2 [5 0] b [5 5])]
             (sut/cut-polygon poly (gl/line2 [5 0] [5 10])))
          "vertical cut")))
  (let [rect (rect/rect 0 0 10 10)
        [a b c d] (g/vertices rect)]
    (t/testing "rectangles"
      (is (= [(gp/polygon2 a b c d)]
             (sut/cut-polygon rect (gl/line2 [0 0] [0 10])))
          "identity if line is coincident with edge")
      (is (= [(gp/polygon2 a b c d)]
             (sut/cut-polygon rect (gl/line2 [0 5] [0 10])))
          "identify if line is partially coincident with edge")
      (is (= [(gp/polygon2 a b [10 5] [0 5])
              (gp/polygon2 [10 5] c d [0 5])]
             (sut/cut-polygon rect (gl/line2 [0 5] [10 5])))
          "horizontal split")
      (is (= [(gp/polygon2 a [5 0] [5 10] d)
              (gp/polygon2 [5 0] b c [5 10])]
             (sut/cut-polygon rect (gl/line2 [5 0] [5 10])))
          "vertical split")
      (is (= [(gp/polygon2 a [5 0] [10 5] c d)
              (gp/polygon2 [5 0] b [10 5])]
             (sut/cut-polygon rect (gl/line2 [5 0] [10 5])))
          "diagonal cut into rectangle missing corner & triangle")
      (is (= [(gp/polygon2 a b d)
              (gp/polygon2 b c d)]
             (sut/cut-polygon rect (gl/line2 [0 10] [10 0])))
          "diagonal cut into two triangles")))
  (let [circle (g/as-polygon (gc/circle 5 5 5) 4)]
    (t/testing "circles"
      ;; FIXME: not sure if working when line is from vertex to vertex
      (is (every? (fn [[a b]] (roughly-same-polygon a b))
                  (map vector
                       [(gp/polygon2 [10 5] [5 10] [0 5] [1 4] [9 4])
                        (gp/polygon2 [1 4] [5 0] [9 4])]
                       (sut/cut-polygon circle (gl/line2 [-1 4] [11 4])))))))
  ;; a --   -- b
  ;; |         |
  ;; | f --- e |
  ;; | |     | |
  ;; h-g     d-c
  (let [poly (gp/polygon2 [0 0] [10 0] [10 10] [8 10] [8 4] [2 4] [2 10] [0 10])
        [a b c d e f g h] (g/vertices poly)]
    (t/testing "concave polygon"
      (is (= [(gp/polygon2 a [5 0] [5 4] f g h)
              (gp/polygon2 [5 0] b c d e [5 4])]
             (sut/cut-polygon poly (gl/line2 [5 0] [5 10])))
          "vertical slice")
      (is (= [(gp/polygon2 a [2 0] g h)
              (gp/polygon2 [2 0] b c d e f)]
             (sut/cut-polygon poly (gl/line2 [2 0] [2 10])))
          "vertical slice, coincident f-g")
      (is (= [(gp/polygon2 a b [10 8] [8 8] e f [2 8] [0 8])
              (gp/polygon2 [2 8] g h [0 8])
              (gp/polygon2 [10 8] c d [8 8])]
             (sut/cut-polygon poly (gl/line2 [0 8] [10 8])))
          "horizontal slice into two polygons")
      (is (= [(gp/polygon2 a b [10 8] [8 8] e f [2 8] [0 8])
              (gp/polygon2 [2 8] g h [0 8])
              (gp/polygon2 [10 8] c d [8 8])]
             (sut/cut-polygon poly (gl/line2 [0 8] [10 8])))
          "horizontal slice into three polygons")
      (is (= [(gp/polygon2 a b [10 4] [0 4])
              (gp/polygon2 f g h [0 4])
              (gp/polygon2 [10 4] c d e)]
             (sut/cut-polygon poly (gl/line2 [0 4] [10 4])))
          "horizontal slice, coincident to f-e")
      (is (= [(gp/polygon2 a b e f g h)
              (gp/polygon2 b c d e)]
             (sut/cut-polygon poly (gl/line2 b e)))
          "trims off a corner connected by two vertices")
      (is (= [poly] (sut/cut-polygon poly (gl/line2 f e)))
          "cutting with an internal coincident horizontal line is identity")
      (is (= [poly] (sut/cut-polygon poly (gl/line2 f g)))
          "cutting with an internal coincident vertical line is identity"))))

(deftest overlapping-polygons
  (is (sut/overlapping-polygon? (rect/rect 10) (rect/rect 10))
      "identity")
  (is (sut/overlapping-polygon? (rect/rect 10) (rect/rect 10 0 10 10))
      "along an edge")
  (is (sut/overlapping-polygon? (rect/rect 10) (rect/rect 5 5 10 10))
      "vertice falls inside")
  (is (sut/overlapping-polygon? (rect/rect 10) (rect/rect 1 1 5 5))
      "contains b")
  (is (sut/overlapping-polygon? (rect/rect 1 1 5 5) (rect/rect 10))
      "encompassed by b")
  (is (sut/overlapping-polygon? (rect/rect 10) (rect/rect 10 10 5 5))
      "single point of contact")
  (is (not (sut/overlapping-polygon? (rect/rect 10) (rect/rect 11 0 10 10)))))

(deftest join-polygons
  (is (tm/delta=
       (gp/polygon2 [0 0] [10 10] [0 10])
       (sut/join-polygons (gp/polygon2 [0 0] [10 10] [0 10])
                          (gp/polygon2 [0 0] [10 10] [0 10])))
      "triangle identity")
  (is (tm/delta=
       (gp/polygon2 [0 0] [10 0] [10 10] [0 10])
       (sut/join-polygons (rect/rect 10) (rect/rect 10)))
      "rectangle identity")
  #_(is (tm/delta=
         (gp/polygon2 [0 0] [10 0] [10 10] [0 10])
         (sut/join-polygons (gp/polygon2 [0 0] [10 0] [10 10])
                            (gp/polygon2 [0 0] [10 10] [0 10])))
        "triangles with coincident edge")
  #_(is (tm/delta=
         (gp/polygon2 [0 0] [10 0] [10 10] [0 10])
         (sut/join-polygons (gp/polygon2 [0 0] [10 10] [0 10])
                            (gp/polygon2 [0 0] [10 0] [10 10])))
        "triangles with coincident edge, swapped")
  #_(is (tm/delta=
         (gp/polygon2 [0 0] [10 0] [10 10] [0 10])
         (sut/join-polygons (gp/polygon2 [0 0] [10 0] [10 10])
                            (gp/polygon2 [10 10] [0 10] [0 0])))
        "triangles with coincident edge, rotated")
  #_(is (tm/delta=
         (gp/polygon2 [0 0] [10 0] [10 10] [0 10])
         (sut/join-polygons (gp/polygon2 [10 10] [0 10] [0 0])
                            (gp/polygon2 [0 0] [10 0] [10 10])))
        "triangles with coincident edge, rotated, swapped")
  (is (tm/delta=
       (gp/polygon2 [-10 0] [10 0] [0 10])
       (sut/join-polygons (gp/polygon2 [0 0] [10 0] [0 10])
                          (gp/polygon2 [0 0] [0 10] [-10 0])))
      "triangles with coincident edge on y-axis")
  (is (tm/delta=
       (gp/polygon2 [-10 0] [10 0] [0 10] [0 20])
       (sut/join-polygons (gp/polygon2 [0 0] [10 0] [0 10])
                          (gp/polygon2 [0 0] [0 20] [-10 0])))
      "triangles with mismatched coincident edge on y-axis")
  (is (tm/delta=
       (gp/polygon2 [-10 0] [10 0] [2 8.88] [2 20])
       (sut/join-polygons (gp/polygon2 [1 0] [10 0] [1 10])
                          (gp/polygon2 [2 0] [2 20] [-10 0]))
       1e-2)
      "triangles with mismatched coincident edge on y-axis")
  (is (tm/delta=
       (gp/polygon2 [0 0] [15 0] [15 10] [10 5] [10 10] [0 10])
       (sut/join-polygons (rect/rect 10)
                          (gp/polygon2 [5 0] [15 0] [15 10])))
      "rectangle with with coincident overlap of triangle")
  (is (tm/delta=
       (gp/polygon2 [0 0] [20 0] [20 10] [0 10])
       (sut/join-polygons (rect/rect 10) (rect/rect 10 0 10 10)))
      "rectangles with coincident edge")
  (is (tm/delta=
       (gp/polygon2 [0 0] [20 0] [20 20] [10 20] [10 10] [0 10])
       (sut/join-polygons (rect/rect 10) (rect/rect 10 0 10 20)))
      "rectangles with partial coincident edge")
  (is (tm/delta=
       (gp/polygon2 [0 0] [20 0] [20 10] [0 10])
       (sut/join-polygons (gp/polygon2 [10 0] [10 10] [0 10] [0 0]) (rect/rect 10 0 10 10)))
      "along an edge, rotated")
  (is (tm/delta=
       (gp/polygon2 [0 0] [10 0] [10 10] [7 10] [7 20] [3 20] [3 10] [0 10])
       (sut/join-polygons (rect/rect 10) (rect/rect 3 10 4 10)))
      "coincident inside an edge")
  (is (tm/delta=
       (gp/polygon2 [0 0] [10 0] [10 10] [7 10] [7 20] [3 20] [3 10] [0 10])
       (sut/join-polygons (rect/rect 10) (rect/rect 3 0 4 20)))
      "coincident, but one side internal")
  (is (tm/delta=
       (gp/polygon2 [0 0] [10 0] [10 10] [0 10])
       (sut/join-polygons (rect/rect 10) (rect/rect 3 0 4 10)))
      "coincident, but all internal")
  (is (tm/delta=
       (gp/polygon2 [0 0] [10 0] [10 5] [15 5] [15 15] [5 15] [5 10] [0 10])
       (sut/join-polygons (rect/rect 10) (rect/rect 5 5 10 10)))
      "overlapping an internal point")
  (is (tm/delta=
       (gp/polygon2 [0 0] [10 0] [10 5] [15 5] [15 15] [5 15] [5 10] [0 10])
       (sut/join-polygons (gp/polygon2 [10 0] [10 10] [0 10] [0 0]) (rect/rect 5 5 10 10)))
      "overlapping an internal point, rotated 1")
  (is (tm/delta=
       (gp/polygon2 [0 0] [10 0] [10 5] [15 5] [15 15] [5 15] [5 10] [0 10])
       (sut/join-polygons (gp/polygon2 [10 10] [0 10] [0 0] [10 0]) (rect/rect 5 5 10 10)))
      "overlapping an internal point, rotated 2")
  (is (tm/delta=
       (gp/polygon2 [0 0] [10 0] [10 5] [15 5] [15 15] [5 15] [5 10] [0 10])
       (sut/join-polygons (gp/polygon2 [0 10] [0 0] [10 0] [10 10]) (rect/rect 5 5 10 10)))
      "overlapping an internal point, rotated 3")
  (is (tm/delta=
       (gp/polygon2 [-10 -10] [0 -10] [0 -5] [5 -5] [5 5] [-3 5] [-5 0] [-10 0])
       (sut/join-polygons (gp/polygon2 [-5 -5] [5 -5] [5 5] [-3 5] [-5 0])
                          (rect/rect -10 -10 10 10)))
      "overlap in -x,-y quadrant")
  (is (nil? (sut/join-polygons (rect/rect 10) (rect/rect 11 0 10 10)))
      "disjoint polygons")
  (is (tm/delta=
       (gp/polygon2 [0 0] [10 0] [10 10] [0 10])
       (sut/join-polygons (rect/rect 10) (rect/rect 1 1 8 8)))
      "a contains b")
  (is (tm/delta=
       (gp/polygon2 [0 0] [10 0] [10 10] [0 10])
       (sut/join-polygons (rect/rect 1 1 8 8) (rect/rect 10)))
      "b contains a"))

(defn edges= [expected actual]
  (->> (map (fn [e a]
              (and (tm/delta= (:segment e) (:segment a))
                   (tm/delta= (:edge-a e) (:edge-a a))
                   (tm/delta= (:edge-b e) (:edge-b a))))
            expected actual)
       (every? true?)))

(deftest coincident-edges
  (is (edges=
       [{:segment [[10 0] [0 10]] :edge-a [[10 0] [0 10]] :edge-b [[10 0] [0 10]]}]
       (sut/coincident-edges (gp/polygon2 [0 0] [10 0] [0 10])
                             (gp/polygon2 [10 0] [0 10] [10 10])))
      "exact coincident edge")
  (is (edges=
       [{:segment [[10 5] [10 0]] :edge-a [[10 0] [10 10]] :edge-b [[10 5] [10 0]]}]
       (sut/coincident-edges (gp/polygon2 [0 0] [10 0] [10 10] [0 10])
                             (gp/polygon2 [10 0] [15 0] [15 5] [10 5])))
      "partial coincident edge")
  (is (empty? (sut/coincident-edges (rect/rect 10) (rect/rect 15 15 1 1)))
      "no coincident edges")
  (is (edges=
       [{:segment [[0 0] [10 0]] :edge-a [[0 0] [10 0]] :edge-b [[0 0] [10 0]]}
        {:segment [[10 0] [10 10]] :edge-a [[10 0] [10 10]] :edge-b [[10 0] [10 10]]}
        {:segment [[10 10] [0 10]] :edge-a [[10 10] [0 10]] :edge-b [[10 10] [0 10]]}
        {:segment [[0 10] [0 0]] :edge-a [[0 10] [0 0]] :edge-b [[0 10] [0 0]]}]
       (sut/coincident-edges (rect/rect 10) (rect/rect 10)))
      "identity, all edges are coincident"))

(deftest slice-polygons
  (is (empty? (sut/slice-polygons [] []))
      "empty, empty")
  (is (empty? (sut/slice-polygons [] [(gl/line2 0 0 10 0)]))
      "empty polygons")
  (is (= [(rect/rect 10 10)] (sut/slice-polygons [(rect/rect 10 10)] []))
      "empty lines")
  (is (= [(gp/polygon2 [0 0] [10 0] [10 5] [0 5])
          (gp/polygon2 [10 5] [10 10] [0 10] [0 5])
          (gp/polygon2 [10 10] [20 10] [20 15] [10 15])
          (gp/polygon2 [20 15] [20 20] [10 20] [10 15])]
         (sut/slice-polygons [(rect/rect 10 10)
                              (rect/rect 10 10 10 10)]
                             [(gl/line2 0 5 20 5)
                              (gl/line2 0 15 20 15)]))
      "horizontal lines")

  (is (= [(gp/polygon2 [0 0] [5 0] [5 10] [0 10])
          (gp/polygon2 [5 0] [10 0] [10 10] [5 10])
          (gp/polygon2 [10 10] [15 10] [15 20] [10 20])
          (gp/polygon2 [15 10] [20 10] [20 20] [15 20])]
         (sut/slice-polygons [(rect/rect 10 10)
                              (rect/rect 10 10 10 10)]
                             [(gl/line2 5 0 5 20)
                              (gl/line2 15 0 15 20)]))
      "vertical lines")

  (is (= [(gp/polygon2 [0 0] [5 0] [5 5] [0 5])
          (gp/polygon2 [5 0] [10 0] [10 5] [5 5])
          (gp/polygon2 [10 5] [10 10] [5 10] [5 5])
          (gp/polygon2 [5 10] [0 10] [0 5] [5 5])
          (gp/polygon2 [10 0] [20 0] [20 5] [10 5])
          (gp/polygon2 [20 5] [20 10] [10 10] [10 5])]
         (sut/slice-polygons [(rect/rect 10 10)
                              (rect/rect 10 0 10 10)]
                             [(gl/line2 0 5 20 5)
                              (gl/line2 5 0 5 20)]))
      "crossed lines"))

(comment (t/run-tests))
