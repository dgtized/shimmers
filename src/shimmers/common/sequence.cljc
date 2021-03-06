(ns shimmers.common.sequence)

(defn index-of
  "Find the index of a particular value in coll."
  [coll value]
  (some (fn [[idx item]] (when (= value item) idx))
        (map-indexed vector coll)))

(defn map-kv
  "Apply f to every value in coll."
  [f coll]
  (reduce-kv (fn [m k v] (assoc m k (f v)))
             (empty coll)
             coll))

(defn mapping
  "Create a mapping of every value in collection to f(value)."
  [f coll]
  (reduce (fn [m x] (assoc m x (f x))) {} coll))

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

;; (weighted [frequency value] ...)
(defn weighted [& options]
  (into [] (mapcat (fn [[frequency value]]
                     (repeat frequency value))
                   (partition 2 options))))

(defn split-by [pred coll]
  ((juxt filter remove) pred coll))
