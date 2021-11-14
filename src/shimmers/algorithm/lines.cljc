(ns shimmers.algorithm.lines
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.utils :as gu]
   [thi.ng.math.core :as tm]))

(defn points->lines [points]
  (map gl/line2 (partition 2 1 points)))

(defn lines->points [lines]
  (->> lines
       (mapcat g/vertices)
       dedupe))

(defn segment-at
  ([line] (segment-at line 0.5))
  ([{[p q] :points} d]
   [(gl/line2 (tm/mix p q 0) (tm/mix p q d))
    (gl/line2 (tm/mix p q d) (tm/mix p q 1.0))]))

(defn segmented
  [{[p q] :points} n]
  {:pre [(pos-int? n)]}
  (for [[a b] (partition 2 1 (tm/norm-range n))]
    (gl/line2 (tm/mix p q a) (tm/mix p q b))))

;; https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
(defn perpendicular-distance [[x1 y1] [x2 y2] [x0 y0]]
  (let [x21 (- x2 x1)
        y21 (- y2 y1)]
    (/ (Math/abs (- (* x21 (- y1 y0))
                    (* (- x1 x0) y21)))
       (Math/sqrt (+ (* x21 x21) (* y21 y21))))))

(defn max-perpendicular-distance [points]
  (let [a (first points)
        b (last points)]
    (loop [i 1 index 0 max-dist 0]
      (if (>= i (dec (count points)))
        [index max-dist]
        (let [pt (nth points i)
              d (perpendicular-distance a b pt)]
          (if (> d max-dist)
            (recur (inc i) i d)
            (recur (inc i) index max-dist)))))))

;; https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm
(defn ramer-douglas-peucker [points epsilon]
  (let [[index max-dist] (max-perpendicular-distance points)]
    (if (> max-dist epsilon)
      (lazy-cat (ramer-douglas-peucker (take index points) epsilon)
                (ramer-douglas-peucker (drop index points) epsilon))
      [(first points) (last points)])))

(defn simplify-line
  "Remove any points that deviate further than `epsilon` distance from the
  perpindicular of the subsection of the line."
  [{:keys [points]} epsilon]
  (gl/linestrip2 (ramer-douglas-peucker points epsilon)))

(defn mix-line
  "Sample two paths at even intervals and return a line-strip with a linear
  interpolation of `factor` between each of the two corresponding points."
  [path-a path-b factor]
  (gl/linestrip2
   (let [samples (max (count (:points path-a))
                      (count (:points path-b)))]
     (for [t (tm/norm-range (dec samples))]
       (tm/mix (g/point-at path-a t) (g/point-at path-b t) factor)))))

(defn dampen
  "Mix each point in a `path` by `factor` with the linear path between the
  beginning and end of `path`."
  [{path :points} factor]
  (gl/linestrip2
   (let [a (first path)
         b (last path)]
     (mapv (fn [pt t] (tm/mix pt (tm/mix a b t) factor))
           path
           (tm/norm-range (dec (count path)))))))

(defn points-between
  "Given a sequence of `points`, and relative offsets `t0` < `t1`, return all
  points that proportionally lie between `t0` and `t1`. The domain of `t0` and
  `t1` is [0.0..1.0], represents the percentage distance along the total arc
  length of the original points."
  [points t0 t1]
  {:pre [(<= t0 t1)]}
  (let [arc-index (gu/arc-length-index points)
        arc-length (last arc-index)]
    (->> (map (fn [p arc] [p (/ arc arc-length)]) points arc-index)
         (drop-while (fn [[_ t]] (< t t0)))
         (take-while (fn [[_ t]] (<= t t1)))
         (map first))))
