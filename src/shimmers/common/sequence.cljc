(ns shimmers.common.sequence)

(defn index-of
  "Find the index of a particular value in coll."
  [coll value]
  (some (fn [[idx item]] (when (= value item) idx))
        (map-indexed vector coll)))

(defn find-first
  [pred coll]
  (reduce (fn [_ x] (when (pred x) (reduced x))) nil coll))

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

(defn sandwich
  "Return a sequence with the `n` edge elements from the original sequence, and
  the inner elements from `replacement`."
  ([xs replacement] (sandwich 1 xs replacement))
  ([n xs replacement]
   (concat (take n xs)
           replacement
           (take-last n xs))))

(defn midsection
  "Return a sequence without the first `n` and last `n` elements."
  ([xs] (midsection 1 xs))
  ([n xs]
   (drop n (drop-last n xs))))

;; Note that int is required because (nth (range 10) 5.5) => 5.5
(defn middle [xs]
  (nth xs (int (/ (count xs) 2))))

(defn centered-range [n]
  (let [elements (inc n)]
    (->> (rest (range elements))
         (mapv #(/ % (double elements))))))

(comment (map (fn [x] [x (centered-range x)]) (range 6)))

(defn rotate
  "Rotate sequence `xs`, `n` steps left if positive, or to the right if negative."
  [n xs]
  (let [s (count xs)]
    (take s (drop (mod n s) (cycle xs)))))

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

(defn first-last [coll]
  [(first coll) (last coll)])

(defn partition-segments
  "Separate `coll` into ranges the length of each `chunks`, padded by `pads`.

  (partition-segments (cycle [1 3]) (cycle [1 2]) (range 12)) =>
  ([0 0] [2 4] [7 7] [9 11])"
  [chunks pads coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [n (first chunks)
           p (take n s)]
       (if (== n (count p))
         (cons (first-last p)
               (partition-segments (rest chunks) (rest pads)
                                   (drop (+ n (first pads)) s)))
         (list (first-last (take n p))))))))

(comment (partition-segments (repeatedly #(+ 4 (rand-int 20)))
                             (repeatedly #(rand-int 4))
                             (range 100)))

(defn collapse
  "Combine consecutive values in `coll` if `collapse?` using `combine`.

  Both `collapse?` and `combine` take the previous value, and the next value."
  [collapse? combine coll]
  (let [step (fn step [xs prev]
               (lazy-seq
                (if-let [s (seq xs)]
                  (if (collapse? prev (first s))
                    (step (rest s) (combine prev (first s)))
                    (cons prev (step (rest s) (first s))))
                  [prev])))]
    (if (seq coll) ;; handle empty collection
      (step (rest coll) (first coll))
      coll)))

(defn iterate-cycles
  "Iterate on `x` using `f` for `n` cycles, returning the final `x`."
  [n f x]
  (nth (iterate f x) n))

(defn iterate-fixed-point
  "Iterate on `x` using `f` until `f` does not change the value of `x`."
  [f x]
  (->> (iterate f x)
       (partition 2 1)
       (take-while (fn [[s s']] (not= s s')))
       last
       first))

(defn retry
  "Retry `retry-fn` until it returns non-nil for up to `tries` attempts."
  [tries retry-fn]
  (let [result (retry-fn)]
    (cond (some? result)
          result
          (= tries 0)
          nil
          :else
          (recur (dec tries) retry-fn))))

(defn pair-cycle [coll]
  (map vector coll (concat (rest coll) (take 1 coll))))

(defn triplet-cycle [coll]
  (partition 3 1 (concat (take-last 1 coll) coll (take 1 coll))))

(comment (pair-cycle [:a :b :c :d])
         (triplet-cycle [:a :b :c :d])
         (triplet-cycle [:a :b :c])
         (triplet-cycle [:a :b])
         (triplet-cycle [:a])
         )
