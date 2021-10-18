(ns shimmers.math.scratch
  (:require [thi.ng.math.core :as tm]))

;; Playing with golden ratio splits
(defn golden [n]
  (drop 1 (map (fn [i] (/ 1.0 (Math/pow tm/PHI i))) (range (inc n)))))

(comment
  (golden 2)
  (golden 3)
  (golden 4))

(comment
  (map #(tm/mix-exp 1.0 32 % 12) (range 0 1 0.05))
  (map #(tm/mix-circular-flipped 0.98 16 %) (range 0 1 0.05))
  (map #(tm/mix-cosine 0.5 2 %) (range 0 1 0.05))
  (map #(tm/mix-lens 0.5 0.1 % 1 100) (range 0 1 0.05))
  (map #(tm/mix-bezier 0.9 3 % 1.5 0.1) (range 0 1 0.05)))
