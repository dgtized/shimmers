(ns shimmers.algorithm.linear-assignment-test
  (:require [shimmers.algorithm.linear-assignment :as sut]
            [clojure.test :as t :refer [deftest is] :include-macros true]
            [thi.ng.geom.vector :as gv]))

(deftest greedy-assignment-match
  (is (= [[(gv/vec2 0 0) (gv/vec2 1 0)]
          [(gv/vec2 0 1) (gv/vec2 1 1)]]
         (sut/greedy-assignment-match <
                                      [(gv/vec2 0 0) (gv/vec2 0 1)]
                                      [(gv/vec2 1 0) (gv/vec2 1 1)]))))
