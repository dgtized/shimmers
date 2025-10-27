(ns shimmers.math.bias-gain
  (:require
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.math.core :as tm]
   [shimmers.common.string :as scs]))

;; http://demofox.org/biasgain.html
(defn bias ^double [^double t ^double amt]
  (/ t (+ (* (- (/ 1.0 amt) 2.0)
             (- 1.0 t))
          1.0)))

(defn cbias ^double [^double t ^double amt]
  (bias (tm/clamp01 t) amt))

(defn gain ^double [^double t ^double amt]
  (if (< t 0.5)
    (/ (bias (* t 2.0) amt) 2.0)
    (+ (/ (bias (- (* t 2.0) 1.0) (- 1.0 amt)) 2.0) 0.5)))

(defn cgain ^double [^double t ^double amt]
  (gain (tm/clamp01 t) amt))

(comment
  (for [t (tm/norm-range 20)]
    (scs/format "%.3f %.3f %.3f"
                (float t)
                (float (bias t 0.15))
                (float (gain t 0.15))))

  (dr/sample-freqs 10000 (fn [] (int (* 10 (bias (dr/random) 0.15))))) ;; bias start
  (dr/sample-freqs 10000 (fn [] (int (* 10 (bias (dr/random) 0.85))))) ;; bias end
  (dr/sample-freqs 10000 (fn [] (int (* 10 (gain (dr/random) 0.15))))) ;; bias edges
  (dr/sample-freqs 10000 (fn [] (int (* 10 (gain (dr/random) 0.85))))) ;; bias middle
  )

(defn bias-mix [^double amt]
  (fn ^double [^double v0 ^double v1 ^double t]
    (tm/mix* v0 v1 (cbias t amt))))

(defn gain-mix [^double amt]
  (fn ^double [^double v0 ^double v1 ^double t]
    (tm/mix* v0 v1 (cgain t amt))))

;; See also https://www.flong.com/archive/texts/code/shapers_circ/index.html
(defn bias-gain
  "Generalized Schlick's Bias/Gain

  `x` is in [0,1] and is the variable being transformed.

  `t` is in [0,1] where if t=0|1, the function reproduces Schlick's bias, and
  when t=1/2 it reproduces Schlick's gain. The value of `t` is also the
  inflection point where the curve flips and y=x=t.

  When `s` is 1, `x` is linear to output, when s<1 it will have low slope in
  middle and steep on both edges, in contrast s>1 has low slope at beginning and
  end and steep in middle.

  https://arxiv.org/pdf/2010.09714"
  [^double x ^double s ^double t]
  (let [epsilon 1e-6]
    (if (< x t)
      (/ (* t x)
         (+ x (* s (- t x)) epsilon))
      (+ 1 (/ (* (- 1 t) (- x 1))
              (+ (- 1 x (* s (- t x)))
                 epsilon))))))

(comment
  (for [s [0.1 0.5 1.0 2.0 10.0]
        t [0.0 0.25 0.5 0.75 1.0]]
    [[s t]
     (for [x (range 0 1 0.025)]
       (bias-gain x s t))]))
