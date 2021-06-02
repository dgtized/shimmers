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

(defn polar [r theta]
  (geom/as-cartesian (tv/vec2 r theta)))

(defn unit2-from-angle [theta]
  (vec2 (Math/cos theta) (Math/sin theta)))

(defn jitter
  "Create a random unit vector and then scale it by `amount` to use as noise."
  [amount]
  (scale (unit2-from-angle (rand (* 2 Math/PI))) amount))

(defn snap-to
  "Snap an input angle `dir` to the closest multiple of `radians`."
  [dir radians]
  (if (> radians 0)
    (-> (geom/heading dir)
        (/ radians)
        Math/round
        (* radians)
        unit2-from-angle)
    dir))
