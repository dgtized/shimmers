(ns shimmers.algorithm.lines
  (:require [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
            [thi.ng.math.core :as tm]))

(defn points->lines [points]
  (map gl/line2 (partition 2 1 points)))

(defn lines->points [lines]
  (->> lines
       (mapcat geom/vertices)
       dedupe))

(defn segment-at
  ([line] (segment-at line 0.5))
  ([{[p q] :points} d]
   [(gl/line2 (tm/mix p q 0) (tm/mix p q d))
    (gl/line2 (tm/mix p q d) (tm/mix p q 1.0))]))

(defn segmented
  [{[p q] :points} n]
  {:pre [(>= n 2)]}
  (for [[a b] (partition 2 1 (tm/norm-range n))]
    (gl/line2 (tm/mix p q a) (tm/mix p q b))))

(comment
  (segmented (gl/line2 [0 0] [0 10]) 2)
  (segmented (gl/line2 [0 0] [1 10]) 3))
