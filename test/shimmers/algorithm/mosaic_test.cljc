(ns shimmers.algorithm.mosaic-test
  (:require [shimmers.algorithm.mosaic :as mosaic]
            [clojure.test :as t :refer [deftest is] :include-macros true]
            [thi.ng.geom.vector :as gv]))

(def seed [{:pos (gv/vec2 0 0) :fill :a}
           {:pos (gv/vec2 1 0) :fill :b}
           {:pos (gv/vec2 1 1) :fill :c}
           {:pos (gv/vec2 0 1) :fill :d}])

(deftest rotate-right
  (is (= [[{:pos (gv/vec2 0 0), :fill :a}
           {:pos (gv/vec2 1 0), :fill :b}
           {:pos (gv/vec2 1 1), :fill :c}
           {:pos (gv/vec2 0 1), :fill :d}]
          [{:pos (gv/vec2 0 0), :fill :d}
           {:pos (gv/vec2 1 0), :fill :a}
           {:pos (gv/vec2 0 1), :fill :c}
           {:pos (gv/vec2 1 1), :fill :b}]
          [{:pos (gv/vec2 0 0), :fill :c}
           {:pos (gv/vec2 1 0), :fill :d}
           {:pos (gv/vec2 0 1), :fill :b}
           {:pos (gv/vec2 1 1), :fill :a}]
          [{:pos (gv/vec2 0 0), :fill :b}
           {:pos (gv/vec2 1 0), :fill :c}
           {:pos (gv/vec2 0 1), :fill :a}
           {:pos (gv/vec2 1 1), :fill :d}]]
         (take 4 (iterate mosaic/rotate-r seed)))))
