(ns shimmers.common.sequence)

(defn rotate
  [n xs]
  (if (>= n 0)
    (->> xs
         cycle
         (drop n)
         (take (count xs)))
    (->> xs
         reverse
         cycle
         (drop (- n))
         (take (count xs))
         reverse)))
