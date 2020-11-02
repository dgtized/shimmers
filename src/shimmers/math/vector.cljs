(ns shimmers.math.vector
  (:require [thi.ng.geom.vector :as tv]
            [thi.ng.math.core :as tm]))

(def vec2 tv/vec2)

(defn add [v1 v2]
  (tm/+ v1 v2))

(defn scale [v n]
  (tm/* v n))

(defn wrap2d [[x y] xmax ymax]
  (vec2 (tm/wrap-range x xmax)
        (tm/wrap-range y ymax)))

(defn constrain2d [[x y] lower upper]
  (vec2 (tm/clamp x lower upper)
        (tm/clamp y lower upper)))
