(ns shimmers.space-colonization-test
  (:require [cljs.test :as t :include-macros true
             :refer-macros [deftest is run-tests testing]]
            [shimmers.math.vector :as v]
            [shimmers.sketches.space-colonization :as sut]))

(deftest closest
  (is (= {:position (v/vec2 2 1)}
         (sut/closest-branch (v/vec2 1 1)
                             [{:position (v/vec2 3 3)}
                              {:position (v/vec2 2 2)}
                              {:position (v/vec2 2 1)}]))))

(comment (run-tests))
