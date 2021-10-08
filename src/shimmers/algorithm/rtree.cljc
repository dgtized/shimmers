(ns shimmers.algorithm.rtree
  (:require [thi.ng.geom.core :as geom]
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
                 (map (fn [s] (make-leaf (geom/bounds s) s))
                      xs))))))

(defn intersects? [a b]
  (if (or (nil? a) (nil? b))
    false
    (geom/intersect-shape a b)))

(defn search-intersection
  [tree box]
  (letfn [(search [{:keys [bounds data children]} box]
            (when (intersects? box bounds)
              (lazy-seq (cons data (mapcat #(search % box) children)))))]
    (remove nil? (search tree box))))
