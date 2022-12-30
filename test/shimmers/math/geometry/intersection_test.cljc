(ns shimmers.math.geometry.intersection-test
  (:require
   [clojure.test :as t :refer [deftest is] :include-macros true]
   [shimmers.math.geometry.intersection :as sut]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.vector :as gv]))

(deftest circle-segment-isec
  (is (sut/circle-segment-intersect? (gc/circle [1 1] 1) (gv/vec2 1 1) (gv/vec2 2 2)))
  (is (sut/circle-segment-intersect? (gc/circle [1 1] 1) (gv/vec2 1 1) (gv/vec2 2 0))))

(deftest circle-ray
  (is (= {:type :impale
          :isec [(gv/vec2 1 0) (gv/vec2 3 0)]
          :points [(gv/vec2 1 0) (gv/vec2 3 0)]}
         (sut/circle-ray (gc/circle [2 0] 1) (gv/vec2) (gv/vec2 5 0))))
  (is (= {:type :impale
          :isec [(gv/vec2 3 0) (gv/vec2 1 0)]
          :points [(gv/vec2 3 0) (gv/vec2 1 0)]}
         (sut/circle-ray (gc/circle [2 0] 1) (gv/vec2 5 0) (gv/vec2))))
  (is (= {:type :exit
          :isec [(gv/vec2 1 0)]
          :points [(gv/vec2 3 0) (gv/vec2 1 0)]}
         (sut/circle-ray (gc/circle [2 0] 1) (gv/vec2 2 0) (gv/vec2))))
  (is (= {:type :poke
          :isec [(gv/vec2 1 0)]
          :points [(gv/vec2 1 0) (gv/vec2 3 0)]}
         (sut/circle-ray (gc/circle [2 0] 1) (gv/vec2) (gv/vec2 1 0))))
  (is (= {:type :past :isec [] :points [(gv/vec2 1 0) (gv/vec2 3 0)]}
         (sut/circle-ray (gc/circle [2 0] 1) (gv/vec2 4 0) (gv/vec2 5 0))))
  (is (= {:type :before :isec [] :points [(gv/vec2 1 0) (gv/vec2 3 0)]}
         (sut/circle-ray (gc/circle [2 0] 1) (gv/vec2 -1 0) (gv/vec2))))
  (is (= {:type :inside :isec [] :points [(gv/vec2 0 0) (gv/vec2 4 0)]}
         (sut/circle-ray (gc/circle [2 0] 2) (gv/vec2 1 0) (gv/vec2 3 0))))
  (is (= {:type :inside :isec [] :points [(gv/vec2 2 -2) (gv/vec2 2 2)]}
         (sut/circle-ray (gc/circle [2 0] 2) (gv/vec2 2 0) (gv/vec2 2 1)))
      "vertical inside")
  ;; FIXME: why discrepency between clj/cljs
  (is (= #?(:cljs {:type :tangent :isec [(gv/vec2 1 0)] :points [(gv/vec2 1 0)]}
            :clj nil)
         (sut/circle-ray (gc/circle [2 0] 1) (gv/vec2 1 0) (gv/vec2 1 1)))))

(comment (t/run-tests))
