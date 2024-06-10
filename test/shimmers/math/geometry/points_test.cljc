(ns shimmers.math.geometry.points-test
  (:require [shimmers.math.geometry.points :as sut]
            [clojure.test :as t :refer [deftest is] :include-macros true]))

(deftest points-delta=
  (is (sut/points-delta= [[(/ 1 3) 0.3] [0.1 0.1]]
                         [[0.333333 0.3] [0.1 0.1]])
      "equals vectors")
  (is (not (sut/points-delta= [] []))
      "vectors contain points")
  (is (not (sut/points-delta= [[(/ 1 3) 0.3] [0.1 0.1] [0.5 0.5]]
                              [[0.333333 0.3] [0.1 0.1]]))
      "extra args for first set")
  (is (not (sut/points-delta= [[(/ 1 3) 0.3] [0.1 0.1]]
                              [[0.333333 0.3] [0.1 0.1] [0.5 0.5]]))
      "extra args for second set"))
