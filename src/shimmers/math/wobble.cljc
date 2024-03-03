(ns shimmers.math.wobble
  (:require [shimmers.math.deterministic-random :as dr]
            [shimmers.math.equations :as eq]))

(defn create
  ([r] (create r (dr/random-tau)))
  ([r c] {:r r :c c}))

(defn sin [{:keys [r c]} t p]
  (Math/sin (+ (* r t) p c)))

(defn cube-sin [{:keys [r c]} t p]
  (eq/cube (Math/sin (+ (* r t) p c))))
