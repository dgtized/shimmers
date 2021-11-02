(ns shimmers.algorithm.polygon-detection-test
  (:require #?(:clj [clojure.test :as t :refer [deftest is are]]
               :cljs [cljs.test :as t :refer-macros [deftest is are] :include-macros true])
            [loom.graph :as lg]
            [shimmers.algorithm.polygon-detection :as sut]
            [shimmers.math.vector :as v]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(deftest ordered-points
  (t/testing "outbound set is empty"
    (is (not (sut/clockwise-point (gv/vec2 1 1) (gv/vec2) [])))
    (is (not (sut/counter-clockwise-point (gv/vec2 1 1) (gv/vec2) []))))
  (t/testing "vector inbound is in outbound set"
    (is (= (gv/vec2 1 1) (sut/clockwise-point (gv/vec2 1 1) (gv/vec2) [(gv/vec2 1 1)])))
    (is (= (gv/vec2 1 1) (sut/counter-clockwise-point (gv/vec2 1 1) (gv/vec2) [(gv/vec2 1 1)]))))
  (t/testing "chooses the remaining point if inbound is in the outbound set"
    (is (= (gv/vec2 0 1) (sut/clockwise-point (gv/vec2 1 1) (gv/vec2) [(gv/vec2 0 1) (gv/vec2 1 1)])))
    (is (= (gv/vec2 0 1) (sut/counter-clockwise-point (gv/vec2 1 1) (gv/vec2) [(gv/vec2 0 1) (gv/vec2 1 1)]))))
  (t/testing "chooses the next closest point if collinear"
    (is (= (gv/vec2 0 1) (sut/clockwise-point (gv/vec2 1 1) (gv/vec2) [(gv/vec2 0 1) (gv/vec2 0 2)])))
    (is (= (gv/vec2 0 2) (sut/clockwise-point (gv/vec2 0 1) (gv/vec2) [(gv/vec2 0 1) (gv/vec2 0 2)])))
    (is (= (gv/vec2 0 1) (sut/clockwise-point (gv/vec2 0 2) (gv/vec2) [(gv/vec2 0 1) (gv/vec2 0 2)])))

    (is (= (gv/vec2 0 1) (sut/counter-clockwise-point (gv/vec2 1 1) (gv/vec2) [(gv/vec2 0 1) (gv/vec2 0 2)])))
    (is (= (gv/vec2 0 2) (sut/counter-clockwise-point (gv/vec2 0 1) (gv/vec2) [(gv/vec2 0 1) (gv/vec2 0 2)])))
    (is (= (gv/vec2 0 1) (sut/counter-clockwise-point (gv/vec2 0 2) (gv/vec2) [(gv/vec2 0 1) (gv/vec2 0 2)]))))
  (let [points (mapv (fn [t] (gv/vec2 (map int (v/polar 1.5 (* tm/TWO_PI t)))))
                     (butlast (tm/norm-range 8)))
        [a b c d e f g h] points]
    (are [point expected]
        (= expected (sut/clockwise-point point (gv/vec2) points))
      h a
      a b
      b c
      c d
      d e
      e f
      f g
      g h)
    (are [point expected]
        (= expected (sut/counter-clockwise-point point (gv/vec2) points))
      b a
      c b
      d c
      e d
      f e
      g f
      h g
      a h)))

(def nodes (map gv/vec2 [[0 0] [10 0] [20 0] [0 10] [10 10] [20 10]]))

(let [[a b c
       d e f] nodes]
  (def simple-loop (sut/edges->graph [[a b] [b c] [c f] [f e] [e d] [d a]]))
  (def bisect2 (lg/add-edges simple-loop [b e (g/dist b e)]))
  (def bisect3 (lg/add-edges simple-loop [c e (g/dist c e)])))

(deftest faces-and-polygons
  (let [[a b c
         d e f] nodes]
    (t/testing "closest edge of face near point"
      (is (= [a b] (sut/face-edge-near-point simple-loop (gv/vec2 2 1))))
      (is (= [b a] (sut/face-edge-near-point simple-loop (gv/vec2 2 -1))))
      (is (= [d a] (sut/face-edge-near-point simple-loop (gv/vec2 1 2))))
      (is (= [a d] (sut/face-edge-near-point simple-loop (gv/vec2 -1 2)))))

    (t/testing "simple loop"
      (is (= [a b c f e d] (sut/polygon-near-point simple-loop (gv/vec2 2 1)))
          "clockwise cycle from a-b")
      (is (= [b a d e f c] (sut/polygon-near-point simple-loop (gv/vec2 2 -1)))
          "counter-clockwise outer cycle from b-a")
      (is (= [d a b c f e] (sut/polygon-near-point simple-loop (gv/vec2 1 2)))
          "clockwise cycle from d-a")
      (is (= [a d e f c b] (sut/polygon-near-point simple-loop (gv/vec2 -1 2)))
          "counter-clockwise cycle from d-a")
      (is (= [a b c f e d] (sut/polygon-near-point simple-loop (gv/vec2 0 0)))
          "upper-left boundary (clockwise)")
      (is (= [f c b a d e] (sut/polygon-near-point simple-loop (gv/vec2 20 10)))
          "lower-right boundary (counter-clockwise)"))

    (t/testing "bisected loop at b-e"
      (is (= [a b e d] (sut/polygon-near-point bisect2 (gv/vec2 4 1)))
          "short clockwise cycle from a-b")
      (is (= [d a b e] (sut/polygon-near-point bisect2 (gv/vec2 1 4)))
          "short clockwise cycle from a-b")
      (is (= [c f e b] (sut/polygon-near-point bisect2 (gv/vec2 18 4)))
          "short clockwise cycle from c-f")
      (is (= [f e b c] (sut/polygon-near-point bisect2 (gv/vec2 15 8)))
          "short clockwise cycle from f-e")
      (is (= [b a d e f c] (sut/polygon-near-point bisect2 (gv/vec2 4 -1)))
          "long counter-clockwise cycle from b-a")
      (is (= [a d e f c b] (sut/polygon-near-point bisect2 (gv/vec2 -1 4)))
          "long counter-clockwise cycle from a-d")
      (is (= [a b e d] (sut/polygon-near-point bisect2 (gv/vec2 0 0)))
          "upper-left boundary (clockwise inner loop)")
      (is (= [c f e b] (sut/polygon-near-point bisect2 (gv/vec2 20 0)))
          "upper-right boundary (clockwise inner loop)")
      (is (= [d e f c b a] (sut/polygon-near-point bisect2 (gv/vec2 0 10)))
          "lower-left boundary (counter-clockwise outer loop)")
      (is (= [f c b a d e] (sut/polygon-near-point bisect2 (gv/vec2 20 10)))
          "lower-right boundary (counter-clockwise outer loop)"))))

(comment (t/run-tests))
