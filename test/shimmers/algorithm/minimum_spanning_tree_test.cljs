(ns shimmers.algorithm.minimum-spanning-tree-test
  (:require [shimmers.algorithm.minimum-spanning-tree :as sut]
            [cljs.test :as t :refer-macros [deftest is] :include-macros true]
            [thi.ng.geom.vector :as gv]))

(deftest kruskals
  (is (= [[:a :b] [:b :c] [:c :d]]
         (sut/kruskal [:a :b :c :d] [[:a :b] [:b :c] [:c :d] [:b :d]])))
  (let [[a b c d] (map gv/vec2 [[0 0] [0 1] [0 3] [1 0]])]
    (is (= [[a b] [a d] [b c]]
           (sut/kruskal-points [a b c d])))))

(deftest prims
  (let [dists {[:a :b] 1
               [:a :c] 3
               [:c :b] 2
               [:c :d] 3}]
    (is (= [[:a :b] [:b :c] [:c :d]]
           (sut/prim (fn [a b] (or (get dists [a b]) (get dists [b a]) 1000))
                     [:a :b :c :d]))))
  (let [[a b c d] (map gv/vec2 [[0 0] [0 1] [0 3] [1 0]])]
    (is (= [[a b] [a d] [b c]]
           (sut/prim-points [a b c d])))))
