(ns shimmers.math.wave
  (:require [clojure.math :as math]))

;; https://en.wikipedia.org/wiki/List_of_periodic_functions
;; Consider implementing cycloid

(defn square
  "Square wave function from -1 to 1 with frequency `f`"
  [f t]
  (+ (* 2 (- (* 2 (math/floor (* f t)))
             (math/floor (* 2 f t)))) 1))

(defn pulse
  [frequency duty-cycle t]
  (let [v (mod (* frequency t) 1.0)]
    (if (< v duty-cycle) 1 -1)))

(defn sawtooth
  "Sawtooth wave function from -1 to 1 over period `p`."
  [p t]
  (let [f (/ t p)]
    (* 2 (- f (math/floor (+ 0.5 f))))))

(defn triangle
  "Linear wave function from -1 to 1 over period `p`."
  [p t]
  (let [f (math/floor (+ (/ (* 2 t) p) 0.5))]
    (* (/ 4 p) (- t (* (/ p 2) f)) (math/pow -1 f))))

(defn triangle01
  "Linear wave function from 0 to 1 over period `p`."
  [p t]
  (* 2 (abs (- (/ t p) (math/floor (+ (/ t p) (/ 1 2)))))))

(comment
  (map (fn [t] [t (square 1.0 t)]) (range -2 2 0.1))
  (map (fn [t] [t (pulse 1.0 0.25 t)]) (range -2 2 0.125))
  (map (fn [t] [t (pulse 1.0 0.75 t)]) (range -2 2 0.125))
  (map (fn [t] [t (pulse 2.0 0.25 t)]) (range -2 2 0.125))
  (map (fn [t] [t (sawtooth 1.0 t)]) (range -2 2 0.1))
  (map (fn [t] [t (triangle01 1 t)]) (range -1 1 0.1)))
