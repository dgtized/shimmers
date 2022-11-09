(ns shimmers.math.geometry.ellipse-test
  (:require
   [clojure.test :as t :refer [deftest is testing] :include-macros true]
   [shimmers.math.geometry.ellipse :as sut]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as v]
   [thi.ng.math.core :as m :refer [TWO_PI]]))

(deftest basics
  (testing "g/IArea"
    (is (m/delta= TWO_PI (g/area (sut/ellipse 2 1)))))

  (testing "g/IBounds"
    (is (= (rect/rect -3 -2 6 4) (g/bounds (sut/ellipse 3 2))))
    (is (= 6 (g/width (sut/ellipse 3 1))))
    (is (= 4 (g/height (sut/ellipse 3 2)))))

  (testing "g/IBoundingCircle"
    (is (= (gc/circle 3) (g/bounding-circle (sut/ellipse 3 2)))))

  (testing "g/IBoundary"
    (let [origin (sut/ellipse 3 2)
          ellipse (sut/ellipse 1 1 2 1)]
      (testing "origin"
        (is (g/contains-point? origin [0 0]))
        (is (g/contains-point? origin [3 0]))
        (is (not (g/contains-point? origin [4 0])))
        (is (g/contains-point? origin [0 2]))
        (is (not (g/contains-point? origin [0 3]))))
      (testing "translated"
        (is (g/contains-point? ellipse [1 1]))
        (is (g/contains-point? ellipse [3 1]))
        (is (not (g/contains-point? ellipse [4 1]))))))

  (testing "g/ICenter"
    (is (= (sut/ellipse 2 1) (g/center (sut/ellipse [1 1] 2 1))))
    (is (= (sut/ellipse [2 2] 1 2) (g/center (sut/ellipse [1 1] 1 2) [2 2])))
    (is (= (v/vec2 1 1) (g/centroid (sut/ellipse [1 1] 1 2)))))

  (testing "g/IVertexAccess"
    (is (m/delta= (g/vertices (gc/circle [1 1] 1))
                  (g/vertices (sut/ellipse [1 1] 1 1))))
    (is (m/delta= [(v/vec2 3 0)] (g/vertices (sut/ellipse 3 2) 1)))
    (is (m/delta= [(v/vec2 3 0) (v/vec2 -3 0)] (g/vertices (sut/ellipse 3 2) 2)))
    (is (m/delta= [(v/vec2 3 0) (v/vec2 0 2) (v/vec2 -3 0) (v/vec2 0 -2)]
                  (g/vertices (sut/ellipse 3 2) 4))))

  (testing "g/IEdgeAccess"
    (is (m/delta= (g/edges (gc/circle [1 1] 1))
                  (g/edges (sut/ellipse [1 1] 1 1))))
    (is (m/delta= [[(v/vec2 3 0) (v/vec2 3 0)]] (g/edges (sut/ellipse 3 2) 1)))
    (is (m/delta= [[(v/vec2 3 0) (v/vec2 -3 0)] [(v/vec2 -3 0) (v/vec2 3 0)]]
                  (g/edges (sut/ellipse 3 2) 2)))
    (is (m/delta= [[(v/vec2 3 0) (v/vec2 0 2)]
                   [(v/vec2 0 2) (v/vec2 -3 0)]
                   [(v/vec2 -3 0) (v/vec2 0 -2)]
                   [(v/vec2 0 -2) (v/vec2 3 0)]]
                  (g/edges (sut/ellipse 3 2) 4)))))

(comment (t/run-tests))
