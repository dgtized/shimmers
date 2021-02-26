(ns shimmers.math.probability)

(defn chance [p]
  (< (rand) p))
