(ns shimmers.math.hexagon
  (:require
   [shimmers.math.vector :as v]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn hexagon [p r]
  (gc/circle p r))

(defn apothem
  "distance from center of each face to center

  Otherwise known as the inset-circle radius."
  [{:keys [r]}]
  (* (/ tm/SQRT3 2) r))

(def ^:const flat-hex-angles (butlast (range 0 tm/TWO_PI (/ tm/TWO_PI 6))))
(def ^:const pointy-hex-angles (mapv (partial + tm/SIXTH_PI) flat-hex-angles))

(defn hexagon->polygon [{:keys [p r]} angles]
  (-> (for [theta angles]
        (v/polar r theta))
      gp/polygon2
      (g/translate p)))

(defn flat-hexagon->polygon [c]
  (hexagon->polygon c flat-hex-angles))

(defn pointy-hexagon->polygon [c]
  (hexagon->polygon c pointy-hex-angles))

;; https://www.redblobgames.com/grids/hexagons/
(defn cube->axial [[x _ z]]
  (gv/vec2 x z))

(defn axial->cube [[q r]]
  (gv/vec3 q (- (- q) r) r))

(defn oddr->cube [[col row]]
  (let [x (- col (/ (- row (bit-and row 1)) 2))
        z row
        y (- x (- z))]
    (gv/vec3 x y z)))

(defn axial-flat->pixel
  "Converts axial coordinates of flat topped hex to a center point of that hex."
  [size [q r]]
  (tm/* (gv/vec2 (* q (/ 3 2))
                 (+ (* q 0.5 (Math/sqrt 3)) (* r (Math/sqrt 3))))
        size))

(defn axial-pointy->pixel
  "Converts axial coordinates of flat topped hex to a center point of that hex."
  [size [q r]]
  (tm/* (gv/vec2 (+ (* q (Math/sqrt 3)) (* r 0.5 (Math/sqrt 3)))
                 (* r (/ 3 2)))
        size))

(defn cube-flat->pixel
  [size cube]
  (axial-flat->pixel size (cube->axial cube)))

(defn cube-pointy->pixel
  [size cube]
  (axial-pointy->pixel size (cube->axial cube)))

;; TODO: Option for flat/pointy?
(defn cube-hexagon [cube r]
  (hexagon (cube-flat->pixel r cube) r))

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

(defn cube-spiral
  "Generate all cube coordinates spiraling out counter-clockwise from `center` to up to `radius`"
  [center radius]
  (apply concat
         (cons [center]
               (for [i (range 1 (inc radius))]
                 (cube-ring center i)))))

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
