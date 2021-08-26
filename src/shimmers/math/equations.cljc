(ns shimmers.math.equations
  "Useful equations")

(defn gaussian
  "Bell curve of magnitude `a`, centered at `b`, width `c`.
  From https://en.wikipedia.org/wiki/Gaussian_function"
  [a b c x]
  (* a (Math/exp (- (/ (Math/pow (- x b) 2)
                       (* 2 (* c c)))))))

(comment
  (map (fn [x] [x (gaussian 1.0 0.5 0.2 x)]) (range 0 1 0.05)))
