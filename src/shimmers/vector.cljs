(ns shimmers.vector)

(defn add [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn scale [[x y] n]
  [(* x n) (* y n)])

(defn wrap-value [x lower upper]
  (cond (< x lower) upper
        (> x upper) lower
        :else x))

(defn limit-value [x lower upper]
  (cond (< x lower) lower
        (> x upper) upper
        :else x))

(defn constrain2d [[x y] lower upper]
  [(limit-value x lower upper)
   (limit-value y lower upper)])
