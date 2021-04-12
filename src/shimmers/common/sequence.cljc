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

(defn map-with-window
  "For every element in coll, and the surrounding window of n elements, return f(x,window).

  The first and last n/2 elements will have share a window with the n/2th
  element or the n/2th element from the end."
  [n f coll]
  (let [chunks (partition (min n (count coll)) 1 coll)
        middle (int (/ n 2))]
    (concat (map (fn [x] (f x (first chunks)))
                 (take middle (first chunks)))
            (map (fn [chunk] (f (nth chunk middle) chunk))
                 chunks)
            (map (fn [x] (f x (last chunks)))
                 (drop (inc middle) (last chunks))))))

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

;; https://stackoverflow.com/questions/4053845/idiomatic-way-to-iterate-through-all-pairs-of-a-collection-in-clojure
(defn all-pairs [coll]
  (when-let [s (next coll)]
    (lazy-cat (for [y s] [(first coll) y])
              (all-pairs s))))
