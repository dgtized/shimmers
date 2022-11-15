(ns shimmers.math.geometry.collisions-test
  (:require
   [clojure.test :as t :refer [deftest is] :include-macros true]
   [shimmers.math.geometry.collisions :as sut]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.vector :as gv]))

(deftest bounded
  (is (sut/bounded? (gc/circle 2) (gv/vec2)))
  (is (not (sut/bounded? (gc/circle 2) (gv/vec2 4 4))))
  (is (sut/bounded? (gc/circle 2) (gc/circle 1)))
  (is (sut/bounded? (gc/circle 2) (gc/circle 2)))
  (is (not (sut/bounded? (gc/circle 2) (gc/circle 3))))
  (is (sut/bounded? (gc/circle 2) (gc/circle [1 0] 1)))
  (is (not (sut/bounded? (gc/circle 2) (gc/circle [1.1 0] 1))))
  (is (not (sut/bounded? (gc/circle 2) (gc/circle [1 0] 2)))))
