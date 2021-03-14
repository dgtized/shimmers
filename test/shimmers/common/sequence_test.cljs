(ns shimmers.common.sequence-test
  (:require [shimmers.common.sequence :as sut]
            [cljs.test :as t :include-macros true]))

(t/deftest rotate
  (t/is (= [1 2 3] (sut/rotate 0 [1 2 3])))
  (t/is (= [2 3 1] (sut/rotate 1 [1 2 3])))
  (t/is (= [3 1 2] (sut/rotate -1 [1 2 3])))
  (t/is (= [3 4 1 2] (sut/rotate 2 [1 2 3 4])))
  (t/is (= [2 3 4 1] (sut/rotate -3 [1 2 3 4]))))

(t/deftest weighted
  (t/is (= [] (sut/weighted)))
  (t/is (= [:a :a] (sut/weighted 2 :a)))
  (t/is (= [:a :b :b] (sut/weighted 1 :a 2 :b))))

(t/deftest separate
  (t/is (= [[] []] (sut/separate #(> % 1) [])))
  (t/is (= [[3 2] [1 0]] (sut/separate #(> % 1) (range 4))))
  (t/is (= [[2 3] [0 1]] (sut/separate #(> % 1) (vec (range 4))))))
