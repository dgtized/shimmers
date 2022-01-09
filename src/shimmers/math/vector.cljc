(ns shimmers.math.vector
  (:require [thi.ng.geom.core :as g]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(def vec2 gv/vec2)
(def vec3 gv/vec3)

(def ^:const up (vec3 0 1 0))
(def ^:const down (vec3 0 -1 0))
(def ^:const left (vec3 -1 0 0))
(def ^:const right (vec3 1 0 0))
(def ^:const forward (vec3 0 0 1))
(def ^:const back (vec3 0 0 -1))

(defn add [v1 v2]
  (tm/+ v1 v2))

(defn wrap2d [[x y] xmax ymax]
  (vec2 (tm/wrap-range x xmax)
        (tm/wrap-range y ymax)))

(defn clamp-bounds [bounds [x y]]
  (vec2 (tm/clamp x (rect/left bounds) (rect/right bounds))
        (tm/clamp y (rect/bottom bounds) (rect/top bounds))))

(defn polar [r theta]
  (g/as-cartesian (vec2 r theta)))

(defn- unit2-from-angle [theta]
  (vec2 (Math/cos theta) (Math/sin theta)))

(defn jitter
  "Create a random unit vector and then scale it by `amount` to use as noise."
  [amount]
  (polar amount (rand tm/TWO_PI)))

(defn snap-to
  "Snap an input angle `dir` to the closest multiple of `radians`."
  [dir radians]
  (if (> radians 0)
    (-> (g/heading dir)
        (/ radians)
        Math/round
        (* radians)
        unit2-from-angle)
    dir))

(defn perp-clockwise [[x y]]
  (vec2 y (- x)))

(defn perp-counter-clockwise [[x y]]
  (vec2 (- y) x))

(defn orientation [[px py] [qx qy] [rx ry]]
  (let [val (- (* (- qy py) (- rx qx))
               (* (- qx px) (- ry qy)))]
    (tm/sign val)))
