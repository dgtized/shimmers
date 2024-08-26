(ns shimmers.scratch.modulus)

(comment
  (for [x (range 10)]
    [x (mod x 5) (mod (- 5 x) 5)])

  (for [s (range 3)
        p (range 2 5)]
    [[s p]
     (mapv (fn [x] (mod (+ s (mod x p)) 5)) (range 5))
     (mapv (fn [x] (mod (+ s (mod (- p x) p)) 5)) (range 5))]))
