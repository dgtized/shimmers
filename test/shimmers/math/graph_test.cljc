(ns shimmers.math.graph-test
  (:require #?(:clj [clojure.test :as t :refer [deftest is]]
               :cljs [cljs.test :as t :include-macros true
                      :refer [deftest is]])
            [shimmers.math.geometry.intersection :as isec]
            [shimmers.math.graph :as sut]
            [thi.ng.geom.vector :as gv]
            [loom.graph :as lg]))

;; a - b   f
;;  \
;; | e |
;;    \
;; d - c   g

(def points (map gv/vec2 [[0 0] [1 0] [1 1] [0 1] [0.5 0.5] [2 0] [2 1]]))
(def graph
  (let [[a b c d] points]
    (sut/edges->graph [[a b] [b c] [c d] [d a] [a c]])))

(deftest planarity
  (is (= 10 (count (lg/edges graph))))
  (is (= 5 (count (sut/unique-edges (lg/edges graph)))))
  (is (= 5 (count (filter (fn [[p q]] (sut/planar-edge? graph p q)) (sut/unique-edges (lg/edges graph))))))
  (let [[a b c d e f g] points]
    (is (= [[a c]]
           (filter (fn [edge] (isec/segment-intersect [b d] edge))
                   (sut/unique-edges (lg/edges graph)))))
    (is (sut/planar-edge? graph a b))
    (is (sut/planar-edge? graph b c))
    (is (sut/planar-edge? graph c d))
    (is (sut/planar-edge? graph d a))
    (is (sut/planar-edge? graph a c))
    (is (not (sut/planar-edge? graph b d)) "crosses a-c")
    (is (not (sut/planar-edge? graph d b)) "crosses a-c")
    (is (not (sut/planar-edge? graph a e)) "coincident with a-c")
    (is (not (sut/planar-edge? graph c e)) "coincident with a-c")
    (is (sut/planar-edge? graph b e))
    (is (sut/planar-edge? graph d e))
    (is (not (sut/planar-edge? graph e f)) "crosses b-c")
    (is (not (sut/planar-edge? graph e g)) "crosses b-c")
    (is (not (sut/planar-edge? graph a f)) "coincident to a-b")
    (is (not (sut/planar-edge? graph a g)) "crosses b-c")
    (is (not (sut/planar-edge? graph d f)) "coincident to d-c")
    (is (not (sut/planar-edge? graph d g)) "crosses b-c")
    (is (sut/planar-edge? graph b f) "coincident to a-b, but *only* at b")
    (is (sut/planar-edge? graph b g))
    (is (sut/planar-edge? graph c f))
    (is (sut/planar-edge? graph c g) "coincident to c-d, but *only* at c")))

(comment (t/run-tests))

