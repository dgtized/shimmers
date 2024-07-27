(ns shimmers.math.interval-test
  (:require
   [clojure.test :as t :refer [deftest is] :include-macros true]
   [shimmers.math.interval :as sut]))

(deftest intersection
  (is (nil? (sut/intersection 0 1 2 3)))
  (is (= [1 1] (sut/intersection 0 1 1 3)))
  (is (= [1 2] (sut/intersection 0 2 1 3)))
  (is (= [1 2] (sut/intersection 0 2 1 2)))
  (is (= [1 2] (sut/intersection 1 2 0 3))))
