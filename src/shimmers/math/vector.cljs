(ns shimmers.math.vector
  (:require [thi.ng.geom.core :as geom]
            [thi.ng.geom.vector :as tv]
            [thi.ng.math.core :as tm]))

(def vec2 tv/vec2)
(def vec3 tv/vec3)

(defn add [v1 v2]
  (tm/+ v1 v2))

(defn sub [v1 v2]
  (tm/- v1 v2))

(defn scale [v n]
  (tm/* v n))

(defn dot [v1 v2]
  (tm/dot v1 v2))

(defn wrap2d [[x y] xmax ymax]
  (vec2 (tm/wrap-range x xmax)
        (tm/wrap-range y ymax)))

(defn constrain2d [[x y] lower upper]
  (vec2 (tm/clamp x lower upper)
        (tm/clamp y lower upper)))

(defn distance [v1 v2]
  (geom/dist v1 v2))

(defn normalize [v]
  (tm/normalize v))
