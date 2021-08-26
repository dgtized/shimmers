(ns shimmers.math.equations
  "Useful equations"
  (:require [thi.ng.math.core :as tm]))

(defn sq [x]
  (* x x))

(defn gaussian
  "Bell curve of magnitude `a`, centered at `b`, width `c`.
  From https://en.wikipedia.org/wiki/Gaussian_function"
  [a b c x]
  (* a (Math/exp (- (/ (sq (- x b))
                       (* 2 (sq c)))))))

(def SQRT_TWO_PI (Math/sqrt tm/TWO_PI))

(defn gaussian-density
  "Probability density function with expected value `mu`, variance `sigma`."
  [mu sigma x]
  (* (/ 1 (* sigma SQRT_TWO_PI))
     (Math/exp (* -0.5 (/ (sq (- x mu))
                          (sq sigma))))))

(comment
  (map (fn [x] [x
               (gaussian 1.0 0.5 0.2 x)
               (gaussian-density 1.0 0.5 x)])
       (range 0 1 0.05)))
