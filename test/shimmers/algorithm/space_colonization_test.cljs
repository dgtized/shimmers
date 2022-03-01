(ns shimmers.algorithm.space-colonization-test
  (:require
   [cljs.test :as t :refer-macros [deftest is] :include-macros true]
   [shimmers.algorithm.space-colonization :as sut]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [thi.ng.math.core :as tm]))

(deftest closest
  (is (= {:position (v/vec2 2 1)}
         (sut/closest-branch (v/vec2 1 1)
                             [{:position (v/vec2 3 3)}
                              {:position (v/vec2 2 2)}
                              {:position (v/vec2 2 1)}]))))

(deftest average-attraction
  (is (= (v/vec2 eq/SQRT2_2 eq/SQRT2_2)
         (sut/average-attraction (sut/->Branch nil (v/vec2))
                                 [(v/vec2 2 2) (v/vec2 2 2)])))
  (let [branch (sut/->Branch nil (v/vec2 100 195))
        attractors [(v/vec2 112.0 189.0) (v/vec2 85.2 182.0) (v/vec2 [91.9 173.5])]]
    (is (tm/delta= (v/vec2 -0.102 -0.994)
                   (sut/average-attraction branch attractors)
                   0.001))))

(comment (t/run-tests))
