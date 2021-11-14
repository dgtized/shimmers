(ns shimmers.algorithm.lines
  (:require [thi.ng.geom.core :as g]
            [thi.ng.geom.line :as gl]
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

(defn simplify-line [{:keys [points]} epsilon]
  (gl/linestrip2 (ramer-douglas-peucker points epsilon)))
