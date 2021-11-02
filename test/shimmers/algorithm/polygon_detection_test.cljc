(ns shimmers.algorithm.polygon-detection-test
  (:require [shimmers.algorithm.polygon-detection :as sut]
            #?(:clj [clojure.test :as t :refer [deftest is are]]
               :cljs [cljs.test :as t :refer-macros [deftest is are] :include-macros true])
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]
            [shimmers.math.vector :as v]))

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

(comment (t/run-tests))
