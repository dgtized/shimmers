(ns shimmers.math.equations
  "Useful equations"
  (:require
   [clojure.math :as math]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.quaternion :as quat]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn unit-cos
  "Cosine function remapped into unit interval [0,1]"
  ^double [^double t]
  (* 0.5 (+ 1 (math/cos t))))

(defn unit-sin
  "Sine function remapped into unit interval [0,1]"
  ^double [^double t]
  (* 0.5 (+ 1 (math/sin t))))

(defn sqr [x]
  (* x x))

(defn cube [x]
  (* x x x))

(defn gaussian
  "Bell curve of magnitude `a`, centered at `b`, width `c`.
  From https://en.wikipedia.org/wiki/Gaussian_function"
  [a b c x]
  (* a (math/exp (- (/ (sqr (- x b))
                       (* 2 (sqr c)))))))

(def ^:const TAU tm/TWO_PI)
(def ^:const SQRT_TWO_PI (math/sqrt tm/TWO_PI))
(def ^:const SQRT2_2 (/ (math/sqrt 2) 2)) ;; 0.707106
(def ^:const SQRT3_2 (/ (math/sqrt 3) 2)) ;; 0.866025

(defn gaussian-density
  "Probability density function with expected value `mu`, variance `sigma`."
  [mu sigma x]
  (* (/ 1 (* sigma SQRT_TWO_PI))
     (math/exp (* -0.5 (/ (sqr (- x mu))
                          (sqr sigma))))))

(comment
  (map (fn [x] [x
               (gaussian 1.0 0.5 0.2 x)
               (gaussian-density 1.0 0.5 x)])
       (range 0 1 0.05)))

;; clothoid spiral
;; https://pwayblog.com/2016/07/03/the-clothoid/ origin is the midpoint where r
;; is infinity and L is 0, and each side of the curve is one of the two circles.
;; https://math.stackexchange.com/questions/1785816/calculating-coordinates-along-a-clothoid-betwen-2-curves
;; https://etrr.springeropen.com/articles/10.1007/s12544-013-0119-8
;; https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6687603/
;;
;; Fitting polygons with piecewise clothoid curves
;; http://www.dgp.toronto.edu/~mccrae/projects/clothoid/sbim2008mccrae.pdf
;;
;; Motion planning application of clothoids:
;; http://vigir.missouri.edu/~gdesouza/Research/Conference_CDs/IEEE_IROS_2009/papers/0552.pdf
;; Finding clothoids
;; https://www.sciencedirect.com/science/article/pii/S0377042704000925

;; https://www.researchgate.net/publication/292669884_The_Clothoid_Computation_A_Simple_and_Efficient_Numerical_Algorithm
(defn clothoid-A [radius length]
  (math/sqrt (* radius length)))

;; `A` is defined above
;; `lambda` is clockwise or counter-clockwise rotation (1,-1)
;; `phi0` is the initial angle for -from this is the angle the line ends up, for a
;; clothoid with flat at origin, it's the angle from the origin.
;; `s` is the position along the arc-curve
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
                                 (gv/vec2 (* Δs (math/cos phi))
                                          (* Δs (math/sin phi))))))
                  pos0
                  (directional (range N))))))

;; from origin spiral into quadrant pointed to from `lambda` dir and `phi0`
(def clothoid (generalized-clothoid tm/+ identity))
;; inner point of spiral is fixed at origin, and the spiral extends around it, ending with angle `phi0`.
(def clothoid-from (generalized-clothoid tm/- reverse))

(comment (clothoid 17.32 60 1000 -1 0 (gv/vec2)))

(defn clothoid-tangent [A lambda s phi0]
  (+ (* lambda (/ (* s s) (* 2 A A)))
     phi0))

(defn clothoid-alpha [lambda L R phi0]
  (+ (* lambda (/ L (* 2 R))) phi0))

(defn clothoid-tau [lambda alpha phi0]
  (let [theta (* lambda (- alpha phi0))]
    (if (>= theta 0)
      theta
      (+ tm/TWO_PI theta))))

(defn clothoid-length [R tau]
  (* 2 R tau))

(defn adsr-envelope
  "Returns a piecewise envelope for the given `attack`, `decay`, `sustain`, `release`.

  That envelope takes parameters `t` since start of envelope, and `pressed`,
  indicating length of time inclusive of sustain time."
  [attack decay sustain release]
  (fn [t pressed]
    (let [peak (tm/smoothstep* 0 attack (min t pressed))
          decayed (if (< peak sustain)
                    peak
                    (+ (* (- peak sustain) (- 1.0 (tm/smoothstep* attack (+ attack decay) (min t pressed))))
                       sustain))]
      (if (< t pressed)
        (if (< t attack)
          peak
          decayed)
        (* decayed (- 1.0 (tm/smoothstep* pressed (+ pressed release) t)))))))

(comment
  (let [envelope (adsr-envelope 10 10 0.5 10)]
    (map (fn [t] [t
                 (int (* 100 (envelope t 5)))
                 (int (* 100 (envelope t 10)))
                 (int (* 100 (envelope t 15)))
                 (int (* 100 (envelope t 20)))
                 (int (* 100 (envelope t 22)))])
         (range 0 35 1))))

(defn slerp [v0 v1 t]
  (let [theta (math/acos (tm/dot v0 v1))
        s-theta (math/sin theta)]
    (tm/+ (tm/* v0 (/ (math/sin (* (- 1.0 t) theta)) s-theta))
          (tm/* v1 (/ (math/sin (* t theta)) s-theta)))))

(defn quat-slerp [v0 v1 t]
  (let [q0 (quat/quat (tm/normalize (gv/vec3 v0)) 0)
        q1 (quat/quat (tm/normalize (gv/vec3 v1)) 0)]
    (:xy (tm/mix q0 q1 t))))

(comment
  (mapv (fn [t]
          (let [v (slerp (gv/vec2 0 1) (gv/vec2 1 0) t)]
            [t v (g/heading v)]))
        (tm/norm-range 10))

  (mapv (fn [t]
          [t (quat-slerp (gv/vec2 0 1) (gv/vec2 1 0) t)])
        (tm/norm-range 10))

  ;; neither are happy for interpolating v2[-1 0] to v2[1 0] ?
  )

(defn cos-similarity
  "Measure similarity between vectors `a`, and `b`.

  Positive indicates similarity, with 1 meaning same direction, 0 indicates
  orthogonal, and -1 indicating opposite direction."
  [a b]
  (/ (tm/dot a b) (* (tm/mag a) (tm/mag b))))

;; https://www.desmos.com/calculator/o5pjuhrxlq
(defn flatstep
  "Sorta an inverse of smoothstep with a slow middle and sharper end slope."
  [t f]
  (* (math/pow t f) (+ (* 3 t t t) (* -3 t t) 1)))

(comment
  (map (fn [x] [x (flatstep x 1.2)]) (range 0 1 0.1)))

(defn inv-smoothstep [x]
  (- 0.5 (math/sin (/ (math/asin (- 1.0 (* 2.0 x))) 3.0))))

;; https://stackoverflow.com/a/34576808/34450
(defn flat-smooth [x]
  (+ x (- x (* (sqr x) (- 3.0 (* 2.0 x))))))

;; https://math.stackexchange.com/a/2529617/903738
(defn stair-sigmoid
  [height hscale hoffset alpha x]
  (let [m (- (/ 1 (+ 1 (math/exp (- alpha)))) 0.5)
        term (- (* hscale x) hoffset 0.5)
        r (- term (math/floor term) 0.5)]
    (* height (+ (math/floor term)
                 (* (/ 1.0 (* 2.0 m))
                    (- (/ 1.0 (+ 1.0 (math/exp (* -2.0 alpha r)))) 0.5))
                 1))))

(comment (map (fn [x] (stair-sigmoid 0.1 10.0 0 1 x))
              (range 0 1 (/ 1.0 30))))
