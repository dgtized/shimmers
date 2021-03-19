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

(defn cycle-next [coll current]
  (->> coll
       (into [])
       cycle
       (drop-while (fn [x] (not= current x)))
       (drop 1)
       first))

;; (weighted [frequency value] ...)
(defn weighted [& options]
  (into [] (mapcat (fn [[frequency value]]
                     (repeat frequency value))
                   (partition 2 options))))

(defn separate
  "Separate `coll` by `pred` in a single pass.

  This allows `pred` to work with side-effects like random values."
  [pred coll]
  (loop [elems coll pos (empty coll) neg (empty coll)]
    (if (seq elems)
      (let [elem (first elems)]
        (if (pred elem)
          (recur (rest elems) (conj pos elem) neg)
          (recur (rest elems) pos (conj neg elem))))
      [pos neg])))
