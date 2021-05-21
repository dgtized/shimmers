(ns shimmers.math.hexagon
  (:require [thi.ng.geom.circle :as gc]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn hexagon [p r]
  (gc/circle p r))

;; https://www.redblobgames.com/grids/hexagons/
(defn cube->axial [[x _ z]]
  [x z])

(defn axial->cube [[q r]]
  [q (- (- q) r) r])

(defn axial->hex
  "Converts axial coordinates to a center point of that hex"
  [size [q r]]
  (tm/* (gv/vec2 (* q (/ 3 2))
                 (+ (* q 0.5 (Math/sqrt 3)) (* r (Math/sqrt 3))))
        size))

(defn neighbors [n]
  (for [x (range (- n) (inc n))
        y (range (max (- n) (- (- x) n))
                 (inc (min n (+ (- x) n))))
        :let [z (- (- x) y)]]
    [x y z]))

(comment (neighbors 1)
         (neighbors 2))
