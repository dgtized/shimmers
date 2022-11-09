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
(defn circle-ray
  [{:keys [p r]} origin dest]
  (let [dir (tm/- dest origin)
        f (tm/- origin p)
        A (tm/dot dir dir)
        B (* 2 (tm/dot f dir))
        C (- (tm/dot f f) (* r r))
        discriminant (- (* B B) (* 4 A C))]
    (cond (= discriminant 0)
          (let [t (/ (- B) (* 2 A))
                hit (tm/+ origin (tm/* dir t))]
            {:type :tangent :isec [hit] :points [hit]})
          (> discriminant 0)
          (let [root-disc (Math/sqrt discriminant)
                t0 (/ (- (- B) root-disc) (* 2 A))
                t1 (/ (+ (- B) root-disc) (* 2 A))
                [t0 t1] (if (< t0 t1) [t0 t1] [t1 t0])
                hit0 (tm/+ origin (tm/* dir t0))
                hit1 (tm/+ origin (tm/* dir t1))]
            (cond (<= 0 t0 1)
                  (if (> t1 1)
                    {:type :poke :isec [hit0] :points [hit0 hit1]}
                    {:type :impale :isec [hit0 hit1] :points [hit0 hit1]})
                  (<= 0 t1 1)
                  {:type :exit :isec [hit1] :points [hit0 hit1]}
                  (and (< t0 0) (< t1 0))
                  {:type :past :isec [] :points [hit0 hit1]}
                  (and (> t0 1) (> t1 1))
                  {:type :before :isec [] :points [hit0 hit1]}
                  :else
                  {:type :inside :isec [] :points [hit0 hit1]})))))

(defn circle-segment-intersect?
  [circle p q]
  (when-let [{:keys [type]} (circle-ray circle p q)]
    (contains? #{:tangent :poke :exit :impale} type)))

(defn circle-segment-overlap?
  [circle p q]
  (when-let [{:keys [type]} (circle-ray circle p q)]
    (contains? #{:tangent :poke :exit :impale :inside} type)))

(defn circle-line-intersect?
  [circle {[p q] :points}]
  (circle-segment-intersect? circle p q))

(defn circle-line-overlap?
  [circle {[p q] :points}]
  (circle-segment-overlap? circle p q))
