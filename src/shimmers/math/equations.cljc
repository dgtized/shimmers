(ns shimmers.math.equations
  "Useful equations"
  (:require [thi.ng.math.core :as tm]))

(defn unit-cos
  "Cosine function remapped into unit interval [0,1]"
  [t]
  (* 0.5 (+ 1 (Math/cos t))))

(defn unit-sin
  "Sine function remapped into unit interval [0,1]"
  [t]
  (* 0.5 (+ 1 (Math/sin t))))

(defn sqr [x]
  (* x x))

(defn gaussian
  "Bell curve of magnitude `a`, centered at `b`, width `c`.
  From https://en.wikipedia.org/wiki/Gaussian_function"
  [a b c x]
  (* a (Math/exp (- (/ (sqr (- x b))
                       (* 2 (sqr c)))))))

(def TAU tm/TWO_PI)
(def SQRT_TWO_PI (Math/sqrt tm/TWO_PI))

(defn gaussian-density
  "Probability density function with expected value `mu`, variance `sigma`."
  [mu sigma x]
  (* (/ 1 (* sigma SQRT_TWO_PI))
     (Math/exp (* -0.5 (/ (sqr (- x mu))
                          (sqr sigma))))))

(comment
  (map (fn [x] [x
               (gaussian 1.0 0.5 0.2 x)
               (gaussian-density 1.0 0.5 x)])
       (range 0 1 0.05)))
