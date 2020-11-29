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

(deftest weights
  (let [root (sut/->Branch nil [0 0] [0 0])
        b1 (sut/->Branch 0 [1 1] [0 0])
        b2 (sut/->Branch 1 [1 1] [0 0])
        b3 (sut/->Branch 1 [1 1] [0 0])]
    (is (= {} (sut/index-children [])))
    (is (= {} (sut/index-children [root])))
    (is (= {root [b1]} (sut/index-children [root b1])))
    (is (= {root [b1] b1 [b2 b3]} (sut/index-children [root b1 b2 b3])))
    (is (= {} (sut/weights [])))
    (is (= {root 0} (sut/weights [root])))
    (is (= {root 0 b1 1} (sut/weights [root b1])))
    (is (= {root 0 b1 1 b2 2 b3 2} (sut/weights [root b1 b2 b3])))))

(comment (run-tests))
