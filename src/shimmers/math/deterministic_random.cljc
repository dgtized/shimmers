(ns shimmers.math.deterministic-random
  (:require [clojure.test.check.random :as tcr]))

(defonce *rng* (atom (tcr/make-random)))

(defn random-seed [n]
  (reset! *rng* (tcr/make-random n)))

(defn drand-double []
  (let [[r1 r2] (tcr/split (deref *rng*))]
    (reset! *rng* r2)
    (tcr/rand-double r1)))

(defn drandom [a b]
  (+ (* (- b a) (drand-double)) a))

(defn drand-int [a b]
  (int (drandom a b)))

(defn drand-nth [coll]
  (nth coll (drand-int 0 (count coll))))

(comment (do (random-seed 1000)
             (repeatedly 10 #(drand-int 0 8)))
         (do (random-seed 10)
             (repeatedly 10 #(drand-nth (range 8))))

         (do (random-seed 6)
             (repeatedly 6 #(drand-double))))
