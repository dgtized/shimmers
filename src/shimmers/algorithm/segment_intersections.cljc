(ns shimmers.algorithm.segment-intersections
  "https://en.wikipedia.org/wiki/Bentley%E2%80%93Ottmann_algorithm"
  (:require [clojure.data.avl :as avl]
            #?(:clj [clojure.data.priority-map :as priority]
               :cljs [tailrecursion.priority-map :as priority])
            [thi.ng.geom.core :as g]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.rect :as rect]))

;; tailrecursion.priority-map is cljs only, hence why this is cljs and not cljc

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

(comment
  (def segments (repeatedly 20 (segment-generator (rect/rect 20))))
  (def segmap (segment-map segments))
  (seq (apply priority/priority-map
              (interleave (keys segmap) (keys segmap)))))
;; Sort by :x and then sort by :y, and something about removing left side segments?

;; (defn event-queue [segments]
;;   (priority/priority-map-keyfn-by))

;; (conj (priority/priority-map) [1 2])

(defn left-endpoint? [point]
  (fn [{:keys [points]}]
    (identical? point (first (sort points)))))

(defn right-endpoint? [point]
  (fn [{:keys [points]}]
    (identical? point (second (sort points)))))

;; WIP
#_(defn crossing-point [seg-a seg-b]
    (when-let [p nil]
      p))

#_(defn segment-intersections [segments]
    (let [segmap (segment-map segments)]
      (loop [events (apply priority/priority-map
                           (interleave (keys segmap) (keys segmap)))
             T (avl/sorted-map)
             intersections []]
        (if (empty? events)
          intersections
          (let [[_ point] (peek events)
                events (pop events)
                segs (get segmap point)]
            (if-let [{[p q] :points :as s} (some (left-endpoint? point) segments)]
              (let [T' (assoc T (:y p) s)
                    above (avl/nearest T' > (:y p))
                    below (avl/nearest T' < (:y p))
                    events (if-let [crossing-ab (crossing-point above below)]
                             (disj events crossing-ab)
                             events)
                    ]
                ))
            (recur events T intersections))))))

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

