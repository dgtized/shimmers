(ns shimmers.algorithm.minimum-spanning-tree
  #?@
  (:clj
   [(:require
     [clojure.data.priority-map :as priority]
     [nifty.disjoint-set :as djs]
     [shimmers.common.sequence :as cs]
     [thi.ng.geom.core :as g])]
   :cljs
   [(:require
     [nifty.disjoint-set :as djs]
     [shimmers.common.sequence :as cs]
     [tailrecursion.priority-map :as priority]
     [thi.ng.geom.core :as g])]))

(defn ranked-edges [points]
  (->> (for [[u v] (cs/all-pairs points)]
         [(g/dist u v) [u v]])
       (sort-by first)
       (map second)))

;; Something off about performance here. Points to edges is <N^2, but pretty
;; close, so maybe sort x/y and find close somehow? On top of that though,
;; kruskal-step is not performing as quickly as prim's.
(defn kruskal
  "Returns list of connecting edges in minimum spanning tree.
  `edges` is a vector of vertex pairs, sorted by edge weight.

  (kruskal [:a :b :c :d] [[:a :b] [:b :c] [:c :d] [:b :d]])
  "
  [vertices edges]
  (letfn [(kruskal-step [forest edges union-set]
            (if (empty? edges)
              forest
              (let [[[u v] & remaining] edges]
                (if (not= (djs/canonical union-set u)
                          (djs/canonical union-set v))
                  (recur (conj forest [u v])
                         remaining
                         (djs/union union-set u v))
                  (recur forest remaining union-set)))))]
    (kruskal-step []
                  edges
                  (apply djs/disjoint-set vertices))))

(comment (kruskal [:a :b :c :d] [[:a :b] [:b :c] [:c :d] [:b :d]]))

(defn kruskal-points
  [points]
  (kruskal points (ranked-edges points)))

(defn prim [distance points]
  (letfn [(prim-update [added vertices weights best-edges]
            (if (empty? vertices)
              [weights best-edges]
              (let [vertex (first vertices)
                    dist (distance vertex added)
                    prior (get weights vertex)]
                (if (and dist prior (< dist prior))
                  (recur added (rest vertices)
                         (assoc weights vertex dist)
                         (assoc best-edges vertex [added vertex]))
                  (recur added (rest vertices) weights best-edges)))))
          (prim-step [forest vertices weights best-edge]
            (if (empty? vertices)
              forest
              (let [[v _] (first weights)
                    edge (get best-edge v)
                    remaining (disj vertices v)
                    [weights' best-edge']
                    (prim-update v remaining (dissoc weights v) (dissoc best-edge v))]
                (recur (conj forest edge)
                       remaining
                       weights'
                       best-edge'))))]
    (let [[vertex & remaining] points]
      (prim-step []
                 (set remaining)
                 (reduce (fn [m p] (assoc m p (distance vertex p)))
                         (priority/priority-map)
                         remaining)
                 (into {} (for [p remaining] {p [vertex p]}))))))

(defn prim-points [points]
  (prim g/dist points))
