(ns shimmers.math.interval)

(defn overlap?
  "Check if range a0:a1 overlaps range b0:b1 on the number line."
  [a0 a1 b0 b1]
  (and (<= a0 b1) (<= b0 a1)))

(defn range-overlap
  [a0 a1 b0 b1]
  (cond (<= a0 b0 b1 a1)
        [b0 b1]
        (<= b0 a0 a1 b1)
        [a0 a1]
        (<= a0 b0 a1 b1)
        [b0 a1]
        (<= b0 a0 b1 a1)
        [a0 b1]))

(comment (range-overlap 0 2 1 3)
         (range-overlap 0 2 1 2)
         (range-overlap 1 2 0 3))
