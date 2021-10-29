(ns shimmers.algorithm.polygon-detection-test
  (:require [shimmers.algorithm.polygon-detection :as sut]
            #?(:clj [clojure.test :as t :refer [deftest is]]
               :cljs [cljs.test :as t :refer-macros [deftest is] :include-macros true])
            [thi.ng.geom.vector :as gv]))

;; Use unit-circle to reason about each test, remember y-axis is flipped
(deftest clockwise-starts
  (is (= (gv/vec2 0 1)
         (sut/clockwise-starts (gv/vec2) [(gv/vec2 0 1) (gv/vec2 -1 1)])))
  (is (= (gv/vec2 1 0)
         (sut/clockwise-starts (gv/vec2) [(gv/vec2 1 0) (gv/vec2 0 1)])))
  (is (= (gv/vec2 1 -1)
         (sut/clockwise-starts (gv/vec2) [(gv/vec2 1 1) (gv/vec2 1 -1)])))
  (is (= (gv/vec2 0 1)
         (sut/clockwise-starts (gv/vec2) [(gv/vec2 -1 1) (gv/vec2 0 1) (gv/vec2 1 0)])))
  (is (= (gv/vec2 -1 0.5)
         (sut/clockwise-starts (gv/vec2) [(gv/vec2 -1 0) (gv/vec2 -1 0.5)])))

  )

(comment (t/run-tests))
