(ns shimmers.math.geometry.collisions-test
  (:require [shimmers.math.geometry.collisions :as sut]
            [clojure.test :as t :refer [deftest is] :include-macros true]
            [thi.ng.geom.circle :as gc]))

(deftest bounded
  (is (sut/bounded? (gc/circle 2) (gc/circle 1)))
  (is (sut/bounded? (gc/circle 2) (gc/circle 2)))
  (is (not (sut/bounded? (gc/circle 2) (gc/circle 3))))
  (is (sut/bounded? (gc/circle 2) (gc/circle [1 0] 1)))
  (is (not (sut/bounded? (gc/circle 2) (gc/circle [1.1 0] 1))))
  (is (not (sut/bounded? (gc/circle 2) (gc/circle [1 0] 2)))))
