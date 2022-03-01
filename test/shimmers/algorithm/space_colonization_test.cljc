(ns shimmers.algorithm.space-colonization-test
  (:require
   #?(:clj [clojure.test :as t :refer [deftest is]]
      :cljs [cljs.test :as t :refer-macros [deftest is] :include-macros true])
   [shimmers.algorithm.space-colonization :as sut]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]))

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
  (let [branch (sut/->Branch nil (v/vec2))
        attractors [(v/vec2 1 0) (v/vec2 0 -1) (v/vec2 [1 -1])]]
    (is (= (v/vec2 eq/SQRT2_2 (- eq/SQRT2_2))
           (sut/average-attraction branch attractors)))))

(comment (t/run-tests))
