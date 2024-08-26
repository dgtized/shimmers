(ns shimmers.scratch.modulus)

(comment
  (for [x (range 10)]
    [x (mod x 5) (mod (- 5 x) 5)])

  (for [s (range 4)
        p (range 2 6)]
    [[s p]
     (mapv (fn [x] (mod (+ s (mod x p)) 8)) (range 8))
     (mapv (fn [x] (mod (+ s (mod (- p 1 x) p)) 8)) (range 8))]))
