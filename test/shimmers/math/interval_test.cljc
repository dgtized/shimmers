(ns shimmers.math.interval-test
  (:require
   [clojure.test :as t :refer [deftest is] :include-macros true]
   [shimmers.math.interval :as sut]))

(deftest overlap?
  (is (not (sut/overlap? 1 2 3 4)) "no overlap")
  (is (sut/overlap? 1 3 3 4) "lower touch")
  (is (sut/overlap? 1 3 2 5) "lower overlap")
  (is (sut/overlap? 2 5 5 6) "upper touch")
  (is (sut/overlap? 2 5 4 6) "upper overlap")
  (is (sut/overlap? 3 4 1 6) "cover first")
  (is (sut/overlap? 2 5 3 4) "cover second"))

(deftest intersection
  (is (nil? (sut/intersection 0 1 2 3)) "no overlap")
  (is (= [1 1] (sut/intersection 0 1 1 3)) "lower touch")
  (is (= [1 2] (sut/intersection 0 2 1 3)) "lower overlap")
  (is (= [1 2] (sut/intersection 0 2 1 2)) "lower cover/touch")
  (is (= [1 2] (sut/intersection 0 3 1 2)) "lower cover")
  (is (= [3 3] (sut/intersection -1 3 3 4)) "upper touch")
  (is (= [3 3] (sut/intersection 3 4 2 3)) "upper touch")
  (is (= [-1 1] (sut/intersection -1 3 -2 1)) "upper overlap")
  (is (= [1 2] (sut/intersection 1 2 0 3)) "upper cover"))
