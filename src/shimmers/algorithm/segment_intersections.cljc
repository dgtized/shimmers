(ns shimmers.algorithm.segment-intersections
  "https://en.wikipedia.org/wiki/Bentley%E2%80%93Ottmann_algorithm"
  (:require [clojure.data.avl :as avl]))

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

