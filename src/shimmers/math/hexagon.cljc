(ns shimmers.math.hexagon
  (:require [thi.ng.geom.circle :as gc]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn hexagon [p r]
  (gc/circle p r))

;; https://www.redblobgames.com/grids/hexagons/
(defn cube->axial [[x _ z]]
  (gv/vec2 x z))

(defn axial->cube [[q r]]
  (gv/vec3 q (- (- q) r) r))

(defn axial->hex
  "Converts axial coordinates to a center point of that hex"
  [size [q r]]
  (tm/* (gv/vec2 (* q (/ 3 2))
                 (+ (* q 0.5 (Math/sqrt 3)) (* r (Math/sqrt 3))))
        size))

(defn cube-direction [dir]
  (-> [(gv/vec3  1 -1  0)
       (gv/vec3  1  0 -1)
       (gv/vec3  0  1 -1)
       (gv/vec3 -1  1  0)
       (gv/vec3 -1  0  1)
       (gv/vec3  0 -1  1)]
      (nth dir)))

(defn cube-neighbor [cube dir]
  (tm/+ cube (cube-direction dir)))

(defn cube-neighbors [cube]
  (mapv (partial cube-neighbor cube) (range 6)))

(comment (cube-neighbor (gv/vec3 0 1 0) 4)
         (cube-neighbors (gv/vec3 1 1 1)))

(defn cube-range
  "Cube coordinates for all hexes within distance `n` of 0,0,0 inclusive."
  [n]
  (for [x (range (- n) (inc n))
        y (range (max (- n) (- (- x) n))
                 (inc (min n (+ (- x) n))))
        :let [z (- (- x) y)]]
    (gv/vec3 x y z)))

(defn axial-range
  "Axial coordinates for all hexes within distance `n` of 0,0 inclusive."
  [n]
  (map cube->axial (cube-range n)))

(comment (cube-range 1)
         (cube-range 2)
         (axial-range 1))
