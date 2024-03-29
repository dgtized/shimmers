(ns shimmers.algorithm.mosaic-test
  (:require [shimmers.algorithm.mosaic :as mosaic]
            [clojure.test :as t :refer [deftest is] :include-macros true]
            [thi.ng.geom.vector :as gv]))

(def seed [{:pos (gv/vec2 0 0) :fill :a}
           {:pos (gv/vec2 1 0) :fill :b}
           {:pos (gv/vec2 0 1) :fill :c}
           {:pos (gv/vec2 1 1) :fill :d}])

(deftest rotate-right
  (is (= [{:pos (gv/vec2 0 0) :fill :c}
          {:pos (gv/vec2 1 0) :fill :a}
          {:pos (gv/vec2 0 1) :fill :d}
          {:pos (gv/vec2 1 1) :fill :b}]
         (mosaic/rotate-r seed)))
  (is (= [{:pos (gv/vec2 0 0) :fill :d}
          {:pos (gv/vec2 1 0) :fill :c}
          {:pos (gv/vec2 0 1) :fill :b}
          {:pos (gv/vec2 1 1) :fill :a}]
         (->> seed
              mosaic/rotate-r
              mosaic/rotate-r)))
  (is (= [{:pos (gv/vec2 0 0) :fill :b}
          {:pos (gv/vec2 1 0) :fill :d}
          {:pos (gv/vec2 0 1) :fill :a}
          {:pos (gv/vec2 1 1) :fill :c}]
         (->> seed
              mosaic/rotate-r
              mosaic/rotate-r
              mosaic/rotate-r)))
  (is (= seed (->> seed
                   mosaic/rotate-r
                   mosaic/rotate-r
                   mosaic/rotate-r
                   mosaic/rotate-r))))

(deftest flip-x
  (is (= [{:pos (gv/vec2 1 0) :fill :a}
          {:pos (gv/vec2 0 0) :fill :b}
          {:pos (gv/vec2 1 1) :fill :c}
          {:pos (gv/vec2 0 1) :fill :d}]
         (mosaic/flip-x seed))))

(deftest flip-y
  (is (= [{:pos (gv/vec2 0 1) :fill :a}
          {:pos (gv/vec2 1 1) :fill :b}
          {:pos (gv/vec2 0 0) :fill :c}
          {:pos (gv/vec2 1 0) :fill :d}]
         (mosaic/flip-y seed))))
