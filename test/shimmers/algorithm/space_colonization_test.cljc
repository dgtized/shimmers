(ns shimmers.algorithm.space-colonization-test
  (:require
   [clojure.test :as t :refer [deftest is] :include-macros true]
   [shimmers.algorithm.space-colonization :as sut]
   [shimmers.math.equations :as eq]
   [thi.ng.geom.vector :as gv]))

(deftest closest
  (is (= {:position (gv/vec2 2 1)}
         (sut/closest-branch (gv/vec2 1 1)
                             [{:position (gv/vec2 3 3)}
                              {:position (gv/vec2 2 2)}
                              {:position (gv/vec2 2 1)}]))))

(deftest average-attraction
  (is (= (gv/vec2 eq/SQRT2_2 eq/SQRT2_2)
         (sut/average-attraction (sut/->Branch nil (gv/vec2))
                                 [(gv/vec2 2 2) (gv/vec2 2 2)])))
  (let [branch (sut/->Branch nil (gv/vec2))
        attractors [(gv/vec2 1 0) (gv/vec2 0 -1) (gv/vec2 [1 -1])]]
    (is (= (gv/vec2 eq/SQRT2_2 (- eq/SQRT2_2))
           (sut/average-attraction branch attractors)))))

(comment (t/run-tests))
