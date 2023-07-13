(ns shimmers.algorithm.linear-assignment
  (:require #?(:clj [clojure.data.priority-map :as priority]
               :cljs [tailrecursion.priority-map :as priority])
            [clojure.math.combinatorics :as mc]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.vector :as gv]))

;; See also https://antimatroid.wordpress.com/2017/03/21/a-greedy-approximation-algorithm-for-the-linear-assignment-problem/

(defn online-match-matrix [comparator as bs]
  {:queue (priority/priority-map-by comparator)
   :edges (mc/cartesian-product (range (count as))
                                (range (count bs)))
   :connected {:a {} :b {}}
   :matches []
   :as as
   :bs bs})

(defn online-match-solver [{:keys [queue connected] :as matrix} steps]
  (if (or (zero? steps) (empty? queue))
    matrix
    (let [[[idx-a idx-b] _] (peek queue)]
      (if (or (get-in connected [:a idx-a])
              (get-in connected [:b idx-b]))
        (recur (update matrix :queue pop) (dec steps))
        (recur (-> matrix
                   (update :queue pop)
                   (assoc :connected (-> connected
                                         (assoc-in [:a idx-a] true)
                                         (assoc-in [:b idx-b] true)))
                   (update :matches conj [idx-a idx-b]))
               (dec steps))))))

(defn online-match-update [{:keys [queue edges as bs] :as matrix} steps]
  (if (empty? edges)
    (online-match-solver matrix steps)
    (let [[edges-batch edges'] (split-at steps edges)
          queue' (reduce (fn [pm [idx-a idx-b]]
                           (assoc pm [idx-a idx-b]
                                  (g/dist (nth as idx-a) (nth bs idx-b))))
                         queue
                         edges-batch)]
      (assoc matrix :queue queue' :edges edges'))))

(defn online-match-solution [{:keys [edges as bs] :as matrix}]
  (let [matrix0 (if (empty? edges)
                  matrix
                  (online-match-update matrix (count edges)))
        matrix' (if (empty? (:queue matrix0))
                  matrix0
                  (online-match-solver matrix0 (count (:queue matrix0))))]
    (->> (:matches matrix')
         (sort-by first)
         (mapv (fn [[idx-a idx-b]] [(nth as idx-a) (nth bs idx-b)])))))

(defn match-matrix [comparator as bs]
  (let [matrix (online-match-matrix comparator as bs)]
    (:queue (online-match-update matrix (count (:edges matrix))))))

(defn greedy-match-loop [queue as bs]
  (loop [queue queue
         connected {:a {} :b {}}
         matches []]
    (if (empty? queue)
      (->> matches
           (sort-by first)
           (mapv (fn [[idx-a idx-b]] [(nth as idx-a) (nth bs idx-b)])))
      (let [[[idx-a idx-b] _] (peek queue)]
        (if (or (get-in connected [:a idx-a]) (get-in connected [:b idx-b]))
          (recur (pop queue) connected matches)
          (recur
           (pop queue)
           (-> connected
               (assoc-in [:a idx-a] true)
               (assoc-in [:b idx-b] true))
           (conj matches [idx-a idx-b])))))))

;; O(n^2 log n)?
(defn greedy-assignment-match
  "Find an approximate bipartite matching between a set of coordinates `as` and `bs`.

  `comparator` can be `<` or `>` to determine if matching is aiming for global
  minimization or maximization of the distance between each pair."
  [comparator as bs]
  (greedy-match-loop (match-matrix comparator as bs) as bs))

(comment
  (greedy-assignment-match
   <
   [(gv/vec2 0 0) (gv/vec2 0 1) (gv/vec2 0 2)]
   [(gv/vec2 1 0) (gv/vec2 1 1) (gv/vec2 1 2)])

  (online-match-solution (online-match-matrix
                          <
                          [(gv/vec2 0 0) (gv/vec2 0 1) (gv/vec2 0 2)]
                          [(gv/vec2 1 0) (gv/vec2 1 1) (gv/vec2 1 2)])))
