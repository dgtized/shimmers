(ns shimmers.math.geometry.intersection
  (:require
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; Need tests, not sure this is always working?
(defn segment-intersect
  "Return intersection point between two point segment pairs.

  Equations from https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line."
  [[[x1 y1] [x2 y2]] [[x3 y3] [x4 y4]]]
  (let [epsilon 0.000000001
        denominator (- (* (- x1 x2) (- y3 y4))
                       (* (- y1 y2) (- x3 x4)))]
    (when (>= (Math/abs denominator) epsilon)
      (let [t (/ (- (* (- x1 x3) (- y3 y4))
                    (* (- y1 y3) (- x3 x4)))
                 denominator)
            u (- (/ (- (* (- x1 x2) (- y1 y3))
                       (* (- y1 y2) (- x1 x3)))
                    denominator))]
        (when (and (> t 0.0) (< t 1.0) (> u 0.0))
          (gv/vec2 (+ x1 (* t (- x2 x1)))
                   (+ y1 (* t (- y2 y1)))))))))

(defn line-intersect
  "Return intersection point between two gl/line2 instances"
  [{line1 :points} {line2 :points}]
  (segment-intersect line1 line2))

;; https://stackoverflow.com/questions/1073336/circle-line-segment-collision-detection-algorithm
;; https://www.scratchapixel.com/lessons/3d-basic-rendering/minimal-ray-tracer-rendering-simple-shapes/ray-sphere-intersection
(defn circle-segment-intersection
  [{:keys [c r]} {[p q] :points}]
  (let [d (tm/- q p)
        f (tm/- p c)
        a (tm/dot d d)
        b (* 2 (tm/dot f d))
        c (- (tm/dot f f) (* r r))
        discriminant (- (* b b) (* 4 a c))]
    (when (>= discriminant 0)
      (let [root-disc (Math/sqrt discriminant)
            reciprocal (/ 1 (* 2 a))
            t1 (* (- (- b) root-disc) reciprocal)
            t2 (* (+ (- b) root-disc) reciprocal)]
        [t1 t2 (tm/+ p (tm/* d t1)) (tm/+ p (tm/* d t2))]))))
