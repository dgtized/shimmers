(ns shimmers.common.sequence-test
  (:require [shimmers.common.sequence :as sut]
            [cljs.test :as t :include-macros true]))

(t/deftest rotate
  (t/is (= [2 3 1] (sut/rotate 1 [1 2 3])))
  (t/is (= [3 1 2] (sut/rotate -1 [1 2 3]))))


