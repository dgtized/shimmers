(ns shimmers.math.hexagon-test
  (:require
   [clojure.test :as t :refer [deftest is] :include-macros true]
   [shimmers.math.hexagon :as hex]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def d= tm/delta=)

(deftest neighborhood
  (is (d= [-1 0 1] (hex/cube-neighbor (gv/vec3) 4)))
  (is (d= [-1 1 1] (hex/cube-neighbor (gv/vec3 0 1 0) 4)))
  (is (d= [[1 -1 0] [1 0 -1] [0 1 -1] [-1 1 0] [-1 0 1] [0 -1 1]]
         (hex/cube-neighbors (gv/vec3 0 0 0))))
  (is (d= [[2 0 1] [2 1 0] [1 2 0] [0 2 1] [0 1 2] [1 0 2]]
         (hex/cube-neighbors (gv/vec3 1 1 1)))))

(deftest rotation
  (is (d= [1 -1 0] (hex/cube-rotate-cw (gv/vec3) (gv/vec3 1 0 -1))))
  (is (d= [0 1 -1] (hex/cube-rotate-ccw (gv/vec3) (gv/vec3 1 0 -1))))
  (is (d= (gv/vec3 3 -2 -1)
         (hex/cube-rotate-cw (gv/vec3) (gv/vec3 2 1 -3))))
  (is (d= (gv/vec3 -1 3 -2)
         (hex/cube-rotate-ccw (gv/vec3) (gv/vec3 2 1 -3)))))

(deftest reflection
  (is (d= (gv/vec3 1 -3 2) (hex/cube-reflect-x (gv/vec3 1 2 -3))))
  (is (d= (gv/vec3 -3 2 1) (hex/cube-reflect-y (gv/vec3 1 2 -3))))
  (is (d= (gv/vec3 2 1 -3) (hex/cube-reflect-z (gv/vec3 1 2 -3)))))

(def ring1 [[-1 0 1] [0 -1 1] [1 -1 0] [1 0 -1] [0 1 -1] [-1 1 0]])
(def ring2 [[-2 0 2] [-1 -1 2] [0 -2 2] [1 -2 1] [2 -2 0] [2 -1 -1]
            [2 0 -2] [1 1 -2] [0 2 -2] [-1 2 -1] [-2 2 0] [-2 1 1]])
(def ring3 [[-3 0 3] [-2 -1 3] [-1 -2 3] [0 -3 3] [1 -3 2] [2 -3 1]
            [3 -3 0] [3 -2 -1] [3 -1 -2] [3 0 -3] [2 1 -3] [1 2 -3]
            [0 3 -3] [-1 3 -2] [-2 3 -1] [-3 3 0] [-3 2 1] [-3 1 2]])

(deftest ring
  (is (empty? (hex/cube-ring (gv/vec3) 0)))
  (is (d= ring1 (hex/cube-ring (gv/vec3) 1)))
  (is (d= ring2 (hex/cube-ring (gv/vec3) 2)))
  (is (d= ring3 (hex/cube-ring (gv/vec3) 3))))

(deftest spiral
  (let [origin [[0 0 0]]]
    (is (d= origin (hex/cube-spiral (gv/vec3) 0)))
    (is (d= [[1 1 1]] (hex/cube-spiral (gv/vec3 1 1 1) 0)))
    (is (d= (concat origin ring1) (hex/cube-spiral (gv/vec3) 1)))
    (is (d= (concat origin ring1 ring2) (hex/cube-spiral (gv/vec3) 2)))))

(deftest ranges
  (is (d= [[0 0 0]] (hex/cube-range 0)))
  (is (d= [[0 0]] (hex/axial-range 0)))
  (is (d= [[-1 0 1] [-1 1 0] [0 -1 1]
           [0 0 0]
           [0 1 -1] [1 -1 0] [1 0 -1]]
          (hex/cube-range 1)))
  (is (d= [[-2 2] [-2 1] [-2 0]
           [-1 2] [-1 1] [-1 0] [-1 -1]
           [0 2] [0 1] [0 0] [0 -1] [0 -2]
           [1 1] [1 0] [1 -1] [1 -2]
           [2 0] [2 -1] [2 -2]]
          (hex/axial-range 2))))

(deftest distance
  (is (d= 0 (hex/cube-distance (gv/vec3) (gv/vec3))))
  (is (every? (fn [p] (d= 1 (hex/cube-distance (gv/vec3) p)))
              (hex/cube-ring (gv/vec3) 1)))
  (is (every? (fn [p] (d= 2 (hex/cube-distance (gv/vec3) p)))
              (hex/cube-ring (gv/vec3) 2))))

(comment (t/run-tests))
