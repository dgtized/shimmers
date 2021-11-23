(ns shimmers.math.equations
  "Useful equations"
  (:require [thi.ng.math.core :as tm]
            [thi.ng.geom.vector :as gv]))

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

(def ^:const TAU tm/TWO_PI)
(def ^:const SQRT_TWO_PI (Math/sqrt tm/TWO_PI))

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

;; Motion planning application of clothoids:
;; http://vigir.missouri.edu/~gdesouza/Research/Conference_CDs/IEEE_IROS_2009/papers/0552.pdf
;; Finding clothoids
;; https://www.sciencedirect.com/science/article/pii/S0377042704000925

;; https://www.researchgate.net/publication/292669884_The_Clothoid_Computation_A_Simple_and_Efficient_Numerical_Algorithm
(defn clothoid-A [radius length]
  (Math/sqrt (* radius length)))

(defn clothoid-phi [A lambda phi0 s]
  (+ (/ (* lambda s s)
        (* 2 A A))
     phi0))

(defn generalized-clothoid [vector-op directional]
  (fn [A L N lambda phi0 pos0]
    (let [Δs (/ L N)]
      (reductions (fn [pos n]
                    (let [phi (clothoid-phi A lambda phi0 (* Δs n))]
                      (vector-op pos
                                 (gv/vec2 (* Δs (Math/cos phi))
                                          (* Δs (Math/sin phi))))))
                  pos0
                  (directional (range N))))))

(def clothoid (generalized-clothoid tm/+ identity))
(def clothoid-from (generalized-clothoid tm/- reverse))

(comment (clothoid 17.32 60 1000 -1 0 (gv/vec2 0 0)))
