(ns shimmers.vector
  (:require [thi.ng.geom.vector :as tv]
            [thi.ng.math.core :as tm]))

(def vec2 tv/vec2)

(defn add [v1 v2]
  (tm/+ v1 v2))

(defn scale [v n]
  (tm/* v n))

(defn wrap-value [x lower upper]
  (cond (< x lower) upper
        (> x upper) lower
        :else x))

(defn limit-value [x lower upper]
  (cond (< x lower) lower
        (> x upper) upper
        :else x))

(defn constrain2d [[x y] lower upper]
  (vec2 (limit-value x lower upper)
        (limit-value y lower upper)))
