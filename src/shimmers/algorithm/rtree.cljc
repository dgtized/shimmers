(ns shimmers.algorithm.rtree
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.utils :as gu]))

;; adapted from
;; https://medium.com/@wetter.j/rtrees-in-clojure-51aa164a6102
;; and https://github.com/wetterj/rtree/blob/master/src/rtree/core.clj
;; using thi.ng.geom.rect as bounding box

(defrecord Node [bounds data children])

(defn make-leaf [bounds data]
  (Node. bounds data nil))

(defn make-branch [children]
  (Node. (gu/coll-bounds (map :bounds children)) nil children))

(defn create
  ([xs] (create {} xs))
  ([opts xs]
   (letfn [(top-down [level m nodes]
             (if (<= (count nodes) m)
               (make-branch nodes)
               (split level m nodes)))
           (split [level m nodes]
             (let [k (quot (+ (count nodes) (dec m)) m)
                   dimension (get [rect/left rect/bottom] (mod level 2))]
               (->> nodes
                    (sort-by (fn [{:keys [bounds]}] (dimension bounds)))
                    (partition k k nil)
                    (map (partial top-down (inc level) m))
                    (into [])
                    make-branch)))]
     (when (not-empty xs)
       (top-down 0 (get opts :max-children 25)
                 (map (fn [s] (make-leaf (g/bounds s) s))
                      xs))))))

#_:clj-kondo/ignore
(comment
  ;; https://tildesites.bowdoin.edu/~ltoma/teaching/cs340/spring08/Papers/Rtree-chap1.pdf
  ;; consider using clojure.zip zippers for tree edits?
  (defn insert
    ([tree o] (insert {} tree o))
    ([opts tree o]
     (let [max-children (get opts :max-children 25)
           box (g/bounds o)
           entry (make-leaf box o)]
       (letfn [(search [{:keys [children] :as node}]
                 (when (seq children)
                   (let [child (apply min-key
                                      (fn [t] (g/area (gu/coll-bounds [(:bounds t) box])))
                                      children)]
                     (lazy-seq (cons node (search child))))))]
         (loop [path (reverse (search tree)) root entry]
           (if (empty? path)
             root
             (let [[node & remaining] path]
               (recur remaining ))))))))

  (comment
    (def tree (create {:max-children 2} (repeatedly 10 #(rect/rect (rand-int 100) (rand-int 100) (rand-int 10) (rand-int 10)))))
    (map (fn [t] [(:bounds t) (count (:children t))]) (insert tree (rect/rect 5 5 1 1)))))

(defn intersects? [a b]
  (if (or (nil? a) (nil? b))
    false
    (g/intersect-shape a b)))

(defn search-intersection
  [tree box]
  (letfn [(search [{:keys [bounds data children]} box]
            (when (intersects? box bounds)
              (lazy-seq (cons data (mapcat #(search % box) children)))))]
    (remove nil? (search tree box))))

(defn path-search
  [{:keys [bounds children] :as node} point]
  (when (g/contains-point? bounds point)
    (lazy-seq (cons node
                    (mapcat #(path-search % point) children)))))
