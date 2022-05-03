(ns shimmers.algorithm.lines-test
  (:require
   [cljs.test :as t :refer-macros [deftest is] :include-macros true]
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
  (is (= [(gl/line2 2 5 3 5) (gl/line2 7 5 8 5)]
         (sut/clip-line (gl/line2 0 5 10 5)
                        (gp/polygon2 [2 0] [8 0] [8 10] [7 10] [7 2] [3 2] [3 10] [2 10])))
      "line segment clips multiple regions of a concave polygon")
  (is (= [(gl/line2 2 2 3 2) (gl/line2 7 2 8 2)]
         (sut/clip-line (gl/line2 0 2 10 2)
                        (gp/polygon2 [2 0] [8 0] [8 10] [7 10] [7 2] [3 2] [3 10] [2 10])))
      "line segment clips multiple regions of a concave polygon including coincident edges")
  (is (= [(gl/line2 2.5 2 3 2) (gl/line2 7 2 8 2)]
         (sut/clip-line (gl/line2 2.5 2 8 2)
                        (gp/polygon2 [2 0] [8 0] [8 10] [7 10] [7 2] [3 2] [3 10] [2 10])))
      "line segment clips coincident edge of concave polygon starting inside")
  (is (= [(gl/line2 2 2 3 2) (gl/line2 7 2 7.5 2)]
         (sut/clip-line (gl/line2 2 2 7.5 2)
                        (gp/polygon2 [2 0] [8 0] [8 10] [7 10] [7 2] [3 2] [3 10] [2 10])))
      "line segment clips coincident edge of concave polygon ending inside")
  )

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
      #_(is (= [(gp/polygon2 a [2 0] g h)
                (gp/polygon2 [2 0] b c d e f #_g)]
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
      #_(is (= [(gp/polygon2 a b [10 4] e f [0 4])
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

(comment (t/run-tests))
