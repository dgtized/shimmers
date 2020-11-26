(ns shimmers.space-colonization-test
  (:require [shimmers.space-colonization :as sut]
            [cljs.test :as t :include-macros true
             :refer-macros [deftest is testing run-tests]]
            [shimmers.math.vector :as v]))

(deftest closest
  (is (= {:position (v/vec2 2 1)}
         (sut/closest-branch (v/vec2 1 1)
                             [{:position (v/vec2 3 3)}
                              {:position (v/vec2 2 2)}
                              {:position (v/vec2 2 1)}]))))

(comment (run-tests))
