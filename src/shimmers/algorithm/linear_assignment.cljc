(ns shimmers.algorithm.linear-assignment
  (:require #?(:clj [clojure.data.priority-map :as priority]
               :cljs [tailrecursion.priority-map :as priority])
            [clojure.math.combinatorics :as mc]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.vector :as gv]))

(defn match-matrix [comparator as bs]
  (reduce (fn [pm [idx-a idx-b]]
            (assoc pm [idx-a idx-b]
                   (g/dist (nth as idx-a) (nth bs idx-b))))
          (priority/priority-map-by comparator)
          (mc/cartesian-product (range (count as))
                                (range (count bs)))))

(defn greedy-match-loop [queue as bs]
  (loop [queue queue
         connected {:a {} :b {}}
         matches []]
    (if (empty? queue)
      (mapv (fn [[idx-a idx-b]] [(nth as idx-a) (nth bs idx-b)]) matches)
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
   [(gv/vec2 1 0) (gv/vec2 1 1) (gv/vec2 1 2)]))
