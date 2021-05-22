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

(defn cube-rotate-cw
  "Rotate cube `pos` around `center` 60 degrees clockwise."
  [center pos]
  (let [[x y z] (tm/- pos center)]
    (tm/+ center (gv/vec3 (- z) (- x) (- y)))))

(defn cube-rotate-ccw
  "Rotate cube `pos` around `center` 60 degrees counter-clockwise."
  [center pos]
  (let [[x y z] (tm/- pos center)]
    (tm/+ center (gv/vec3 (- y) (- z) (- x)))))

(comment (cube-rotate-cw (gv/vec3) (gv/vec3 2 1 -3))
         (cube-rotate-ccw (gv/vec3) (gv/vec3 2 1 -3)))

(defn cube-reflect-x
  "Reflect cube `pos` over the x-axis through `center`."
  ([pos] (cube-reflect-x (gv/vec3) pos))
  ([center pos]
   (let [[x y z] (tm/- pos center)]
     (gv/vec3 x z y))))

(defn cube-reflect-y
  "Reflect cube `pos` over the y-axis through `center`."
  ([pos] (cube-reflect-y (gv/vec3) pos))
  ([center pos]
   (let [[x y z] (tm/- pos center)]
     (gv/vec3 z y x))))

(defn cube-reflect-z
  "Reflect cube `pos` over the z-axis through `center`."
  ([pos] (cube-reflect-z (gv/vec3) pos))
  ([center pos]
   (let [[x y z] (tm/- pos center)]
     (gv/vec3 y x z))))

(comment (cube-reflect-x (gv/vec3 1 2 -3))
         (cube-reflect-y (gv/vec3 1 2 -3))
         (cube-reflect-z (gv/vec3 1 2 -3)))

;; TODO test better?
;; TODO try converting to initial direction 0 and possibly clockwise rings?
(defn cube-ring
  "Generate all cube coordinates in a counter-clockwise ring of `radius` around
  `center`. Radius is represented as an integer."
  [center radius]
  (->> [(tm/+ center (tm/* (cube-direction 4) radius)) 0]
       (iterate (fn [[c i]]
                  [(cube-neighbor c (mod (quot i radius) 6)) (inc i)]))
       (take (* 6 radius))
       (map first)))

(comment (cube-ring (gv/vec3) 1)
         (cube-ring (gv/vec3) 2)
         (cube-ring (gv/vec3) 3))

(defn cube-spiral
  "Generate all cube coordinates spiraling out counter-clockwise from `center` to up to `radius`"
  [center radius]
  (apply concat
         (cons [center]
               (for [i (range 1 (inc radius))]
                 (cube-ring center i)))))

(comment (cube-spiral (gv/vec3) 0)
         (cube-spiral (gv/vec3) 1)
         (cube-spiral (gv/vec3) 2)
         (cube-spiral (gv/vec3) 3))

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
