(ns shimmers.algorithm.lines
  (:require [thi.ng.geom.line :as gl]
            [thi.ng.math.core :as tm]))

(defn points->lines [points]
  (map gl/line2 (partition 2 1 points)))

(defn segment-at
  ([line] (segment-at line 0.5))
  ([{[p q] :points} d]
   [(gl/line2 (tm/mix p q 0) (tm/mix p q d))
    (gl/line2 (tm/mix p q d) (tm/mix p q 1.0))]))
