(ns shimmers.algorithm.helpers)

(defn map-kv
  "Apply f to every value in coll."
  [f coll]
  (reduce-kv (fn [m k v] (assoc m k (f v)))
             (empty coll)
             coll))

(defn index-of
  "Find the index of a particular value in coll."
  [coll value]
  (some (fn [[idx item]] (when (= value item) idx))
        (map-indexed vector coll)))
