(ns shimmers.math.hexagon-test
  (:require #?(:clj [clojure.test :as t :refer [deftest is]]
               :cljs [cljs.test :as t :include-macros true
                      :refer [deftest is]])
            [shimmers.math.hexagon :as hex]
            [thi.ng.geom.vector :as gv]))

(deftest rotation
  (is (= (gv/vec3 3 -2 -1)
         (hex/cube-rotate-cw (gv/vec3) (gv/vec3 2 1 -3))))
  (is (= (gv/vec3 -1 3 -2)
         (hex/cube-rotate-ccw (gv/vec3) (gv/vec3 2 1 -3)))))

(deftest reflection
  (is (= (gv/vec3 1 -3 2) (hex/cube-reflect-x (gv/vec3 1 2 -3))))
  (is (= (gv/vec3 -3 2 1) (hex/cube-reflect-y (gv/vec3 1 2 -3))))
  (is (= (gv/vec3 2 1 -3) (hex/cube-reflect-z (gv/vec3 1 2 -3)))))

(deftest ring
  (is (= [[-1 0 1] [0 -1 1] [1 -1 0] [1 0 -1] [0 1 -1] [-1 1 0]]
         (hex/cube-ring (gv/vec3) 1)))
  (is (= [[-2 0 2] [-1 -1 2]
          [0 -2 2] [1 -2 1]
          [2 -2 0] [2 -1 -1]
          [2 0 -2] [1 1 -2]
          [0 2 -2] [-1 2 -1]
          [-2 2 0] [-2 1 1]]
         (hex/cube-ring (gv/vec3) 2)))
  (is (= [[-3 0 3] [-2 -1 3] [-1 -2 3]
          [0 -3 3] [1 -3 2] [2 -3 1]
          [3 -3 0] [3 -2 -1] [3 -1 -2]
          [3 0 -3] [2 1 -3] [1 2 -3]
          [0 3 -3] [-1 3 -2] [-2 3 -1]
          [-3 3 0] [-3 2 1] [-3 1 2]]
         (hex/cube-ring (gv/vec3) 3))))

(deftest spiral
  (is (= [[0 0 0]] (hex/cube-spiral (gv/vec3) 0)))
  (is (= [[0 0 0] [-1 0 1] [0 -1 1] [1 -1 0] [1 0 -1] [0 1 -1] [-1 1 0]]
         (hex/cube-spiral (gv/vec3) 1)))
  (is (= [[0 0 0]

          [-1 0 1] [0 -1 1] [1 -1 0] [1 0 -1] [0 1 -1] [-1 1 0]

          [-2 0 2] [-1 -1 2] [0 -2 2] [1 -2 1] [2 -2 0] [2 -1 -1]
          [2 0 -2] [1 1 -2] [0 2 -2] [-1 2 -1] [-2 2 0] [-2 1 1]]
         (hex/cube-spiral (gv/vec3) 2))))

(deftest ranges
  (is (= [[0 0 0]] (hex/cube-range 0)))
  (is (= [[0 0]] (hex/axial-range 0)))
  (is (= [[-1 0 1] [-1 1 0] [0 -1 1]
          [0 0 0]
          [0 1 -1] [1 -1 0] [1 0 -1]]
         (hex/cube-range 1)))
  (is (= [[-2 2] [-2 1] [-2 0]
          [-1 2] [-1 1] [-1 0] [-1 -1]
          [0 2] [0 1] [0 0] [0 -1] [0 -2]
          [1 1] [1 0] [1 -1] [1 -2]
          [2 0] [2 -1] [2 -2]]
         (hex/axial-range 2))))

(comment (t/run-tests))
