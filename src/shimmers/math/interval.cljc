(ns shimmers.math.interval)

;; experimenting with some interval math helpers

(defn min-range
  [[a0 _] [b0 _]]
  (min a0 b0))

(defn max-range
  [[_ a1] [_ b1]]
  (max a1 b1))

(defn min-max-cover [a b]
  [(min-range a b) (max-range a b)])

(defn overlap?
  "Check if range a0:a1 overlaps range b0:b1 on the number line."
  ([[a0 a1] [b0 b1]]
   (overlap? a0 a1 b0 b1))
  ([a0 a1 b0 b1]
   (and (<= a0 b1) (<= b0 a1))))

(defn intersection
  ([[a0 a1] [b0 b1]]
   (intersection a0 a1 b0 b1))
  ([a0 a1 b0 b1]
   (cond (<= a0 b0 b1 a1)
         [b0 b1]
         (<= b0 a0 a1 b1)
         [a0 a1]
         (<= a0 b0 a1 b1)
         [b0 a1]
         (<= b0 a0 b1 a1)
         [a0 b1])))

(comment (intersection 0 1 1 3)
         (intersection 0 2 1 3)
         (intersection 0 2 1 2)
         (intersection 1 2 0 3))

(defn overlap-range
  ([[a0 a1] [b0 b1]]
   (overlap-range a0 a1 b0 b1))
  ([a0 a1 b0 b1]
   (when-let [hit (intersection a0 a1 b0 b1)]
     (let [[a b] hit]
       (when (< a b)
         [a b])))))
