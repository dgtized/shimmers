(ns shimmers.math.graph-test
  (:require #?(:clj [clojure.test :as t :refer [deftest is]]
               :cljs [cljs.test :as t :include-macros true
                      :refer [deftest is]])
            [shimmers.math.geometry.intersection :as isec]
            [shimmers.math.graph :as sut]
            [thi.ng.geom.vector :as gv]
            [loom.graph :as lg]))

(def points (map gv/vec2 [[0 0] [1 0] [1 1] [0 1]]))
(def graph
  (let [[a b c d] points]
    (sut/edges->graph [[a b] [b c] [c d] [d a] [a c]])))

(deftest planarity
  (is (= 10 (count (lg/edges graph))))
  (is (= 5 (count (sut/unique-edges (lg/edges graph)))))
  (is (= 5 (count (filter (fn [[p q]] (sut/planar-edge? graph p q)) (sut/unique-edges (lg/edges graph))))))
  (let [[a b c d] points
        e (gv/vec2 0.5 0.5)
        f (gv/vec2 2 0)
        g (gv/vec2 2 1)]
    (is (= [[a c]]
           (filter (fn [edge] (isec/segment-intersect [b d] edge))
                   (sut/unique-edges (lg/edges graph)))))
    (is (sut/planar-edge? graph a b))
    (is (sut/planar-edge? graph b c))
    (is (sut/planar-edge? graph c d))
    (is (sut/planar-edge? graph d a))
    (is (sut/planar-edge? graph a c))
    (is (not (sut/planar-edge? graph b d)))
    (is (every? (fn [n] (sut/planar-edge? graph e n)) (lg/nodes graph)))
    (is (not (sut/planar-edge? graph e f)))
    (is (not (sut/planar-edge? graph e g)))
    (is (sut/planar-edge? graph a f))
    (is (not (sut/planar-edge? graph a g)))))

(comment (t/run-tests))

