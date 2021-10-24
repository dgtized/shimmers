(ns shimmers.algorithm.segment-intersections
  "https://en.wikipedia.org/wiki/Bentley%E2%80%93Ottmann_algorithm"
  (:require [clojure.data.avl :as avl]
            [tailrecursion.priority-map :as priority]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.rect :as rect]))

;; focusing on integer coordinates for now
(defn segment-generator [rect]
  (fn []
    (let [p (map int (g/random-point-inside rect))
          q (map int (g/random-point-inside rect))]
      (gl/line2 p q))))

(defn segment-map
  "Generates a mapping from point to segments that hit that point"
  [segments]
  (reduce (fn [m {[p q] :points :as s}]
            (-> m
                (update p (fnil conj #{}) s)
                (update q (fnil conj #{}) s)))
          {} segments))

(comment (segment-map (repeatedly 20 (segment-generator (rect/rect 20)))))

;; Sort by :x and then sort by :y, and something about removing left side segments?

;; (defn event-queue [segments]
;;   (priority/priority-map-keyfn-by))

;; (conj (priority/priority-map) [1 2])

;; (defn segment-intersections [segments]
;;   (let [events (apply priority/priority-map-keyfn-by
;;                       )]))

;; Some AVL tree tests
(comment (def t (apply avl/sorted-map (interleave (map (partial * 10) (range 10)) (reverse (range 10)))))
         (avl/rank-of t 20) ;; index-of
         (avl/nearest t < 20)
         (avl/nearest t > 20)
         (avl/split-key 20 t)
         (avl/split-at 3 t)
         (avl/subrange t >= 20 < 40)
         (nth t 5)
         )

