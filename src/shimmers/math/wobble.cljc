(ns shimmers.math.wobble
  (:require [shimmers.math.deterministic-random :as dr]
            [shimmers.math.equations :as eq]))

(defn create
  ([r] (create r (dr/random-tau)))
  ([r c] {:r r :c c}))

(defn sin [{:keys [r c]} p t]
  (Math/sin (+ (* r t) p c)))

(defn tsin [{:keys [r c]} p t]
  (Math/sin (* eq/TAU (+ (* r t) p c))))

(defn cube-sin [osc p t]
  (eq/cube (sin osc p t)))

(defn cube-tsin [osc p t]
  (eq/cube (tsin osc p t)))
