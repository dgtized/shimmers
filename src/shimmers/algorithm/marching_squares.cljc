(ns shimmers.algorithm.marching-squares
  "https://en.wikipedia.org/wiki/Marching_squares"
  (:require [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn corners [[x y] [xs ys]]
  [(gv/vec2 x y)
   (gv/vec2 (+ x xs) y)
   (gv/vec2 (+ x xs) (+ y ys))
   (gv/vec2 x (+ y ys))])

(defn corner-values
  "Lookup values at each corner of cell starting from upper-left and going
  clockwise to bottom left."
  [lookup corners]
  (for [[i j] corners]
    (lookup i j)))

;; (.toString (bit-shift-left (bit-or 0 1) 1) 2)

(defn threshold [values cutoff]
  (bit-shift-right
   (reduce (fn [binary v]
             (let [r (if (>= v cutoff) 1 0)]
               (bit-shift-left (bit-or binary r) 1)))
           0 values)
   1))

(comment (threshold [0.4 0.5 0.2 0.1] 0.5)
         (threshold [0.4 0.4 0.2 0.1] 0.5))

(defn avg [a b]
  (/ (+ a b) 2))

(defn iso-lookup [[nw ne se sw] [a b c d] bit-value]
  (let [north (tm/mix nw ne (avg a b))
        east (tm/mix ne se (avg b c))
        south (tm/mix sw se (avg c d))
        west (tm/mix nw sw (avg d a))]
    (case bit-value
      0 []
      1 [[west south]]
      2 [[south east]]
      3 [[west east]]
      4 [[north east]]
      5 [[west north]
         [south east]]
      6 [[north south]]
      7 [[west north]]
      8 [[west north]]
      9 [[north south]]
      10 [[west south]
          [north east]]
      11 [[north east]]
      12 [[east west]]
      13 [[south east]]
      14 [[west south]]
      15 [])))

(defn lines [[x y] [sx sy] lookup cutoff]
  (let [corners (corners [x y] [sx sy])
        values (corner-values lookup corners)
        bit-value (threshold values cutoff)]
    (iso-lookup corners values bit-value)))
