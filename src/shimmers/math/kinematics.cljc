(ns shimmers.math.kinematics
  (:require [shimmers.math.equations :as eq]))

;; https://www.youtube.com/watch?v=v1V3T5BPd7E
;; suvat equations
;; s : displacement
;; u : initial velocity aka v_0
;; v : final velocity aka v_f
;; a : acceleration
;; t : time

;; Limitations: only works for constant acceleration in one axis

;; s = (u + v)/2 * t
(defn displacement-from-velocity-change [u v t]
  (* (/ (+ u v) 2) t))

;; v = u + at
(defn velocity-from-acceleration [u a t]
  (+ u (* a t)))

;; s = ut + (at^2)/2
(defn displacement-from-initial-velocity [u a t]
  (+ (* u t) (/ (* a (eq/sqr t)) 2)))

;; s = vt - at^2
(defn displacement-from-final-velocity [v a t]
  (- (* v t) (/ (* a (eq/sqr t)) 2)))

;; v^2 = u^2 + 2as
;; or v = sqrt(u^2+2as)
(defn velocity-from-displacement [u a s]
  (Math/sqrt (+ (eq/sqr u) (* 2 a s))))




