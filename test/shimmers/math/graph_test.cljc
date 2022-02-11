(ns shimmers.math.graph-test
  (:require #?(:clj [clojure.test :as t :refer [deftest is]]
               :cljs [cljs.test :as t :include-macros true
                      :refer [deftest is]])
            [shimmers.math.graph :as sut]
            [thi.ng.geom.vector :as gv]
            [loom.graph :as lg]))

(def graph
  (let [[a b c d] (map gv/vec2 [[0 0] [1 0] [1 1] [0 1]])]
    (sut/edges->graph [[a b] [b c] [c d] [d a] [a c]])))

(deftest planar
  (is (= 10 (count (lg/edges graph))))
  (is (= 5 (count (sut/unique-edges (lg/edges graph)))))
  (is (= 5 (count (filter (fn [[p q]] (sut/planar-edge? graph p q)) (sut/unique-edges (lg/edges graph))))))
  #_(is (not (sut/planar-edge? graph (gv/vec2 0 1) (gv/vec2 1 0))))
  )

(comment (t/run-tests))

