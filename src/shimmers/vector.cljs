(ns shimmers.vector)

(defn add [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn scale [[x y] n]
  [(* x n) (* y n)])
