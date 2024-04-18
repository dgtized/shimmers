(ns shimmers.math.probability
  (:require
   [kixi.stats.distribution :as ksd]
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.math.core :as tm]))

(defn chance [prob]
  (< (rand) prob))

(defn happensity
  "Returns an even distribution with with probability `prob`, otherwise returns 0."
  [prob]
  (if (chance prob)
    (rand)
    0))

(defn mapcat-random-sample
  "Apply `xf` to the subset of `coll` selected with probability density `pf` for
  each element, with the unsampled elements intermingled as before. `xf` must
  return a sequence."
  [pf xf coll]
  (mapcat (fn [x]
            (if (chance (pf x)) (xf x) [x]))
          coll))

(defn map-rand-nth
  "Apply `xf` to update a random element in-place in `coll`."
  [xf coll]
  (let [k (rand-int (count coll))]
    (map-indexed (fn [i el] (if (= i k) (xf el) el))
                 coll)))

(defn mapcat-rand-nth
  "Apply `xf` to expand a random element in-place in `coll`.

  `xf` must return a sequence."
  [xf coll]
  (let [k (rand-int (count coll))]
    (->> coll
         (map-indexed (fn [i el] (if (= i k) (xf el) [el])))
         (apply concat))))

(comment (map-rand-nth (partial * 10) (range 10))
         (mapcat-rand-nth (fn [x] [(* x 10) (* x 100)]) (range 10)))

;; alternatively named cond-if?
;; however this does not address mapping n collections together?
(defn prob-if
  ([prob-fn transform-fn] (prob-if prob-fn transform-fn identity))
  ([prob-fn transform-fn identity-fn]
   (fn [x]
     (if (chance (prob-fn x))
       (transform-fn x)
       (identity-fn x)))))

(comment (map (prob-if (constantly 0.1) inc) (range 10))
         (mapcat (prob-if (constantly 0.1) (fn [x] [(inc x) x]) vector) (range 10))
         ;; This part doesn't error but is not working as expected.
         (map (prob-if (constantly 0.2) (fn [x y] [:v x y]) vector) (range 5) (reverse (range 5))))

(defn gaussian [mu sd]
  (ksd/draw (ksd/normal {:mu mu :sd (+ tm/*eps* (abs sd))})))

(defn gaussian-clamped [mean sd]
  (let [dist (ksd/normal {:mu mean :sd (+ tm/*eps* (abs sd))})]
    (fn []
      (-> dist
          ksd/draw
          (tm/clamp 0 1)))))
