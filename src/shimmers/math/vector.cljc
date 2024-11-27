(ns shimmers.math.vector
  (:require
   [clojure.math :as math]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def v2 gv/vec2)
(def v3 gv/vec3)

(def ^:const up (v3 0 1 0))
(def ^:const down (v3 0 -1 0))
(def ^:const left (v3 -1 0 0))
(def ^:const right (v3 1 0 0))
(def ^:const forward (v3 0 0 1))
(def ^:const back (v3 0 0 -1))

(defn add [v1 v2]
  (tm/+ v1 v2))

(defn wrap2d [[x y] xmax ymax]
  (v2 (tm/wrap-range x xmax)
      (tm/wrap-range y ymax)))

(defn clamp-bounds [bounds [x y]]
  (v2 (tm/clamp x (rect/left bounds) (rect/right bounds))
      (tm/clamp y (rect/bottom bounds) (rect/top bounds))))

(defn polar
  ([theta] (polar 1.0 theta))
  ([r theta]
   (v2 (* r (math/cos theta))
       (* r (math/sin theta)))))

(defn +polar [p r theta]
  (tm/+ p (polar r theta)))

(defn -polar [p r theta]
  (tm/- p (polar r theta)))

(defn snap-to
  "Snap an input angle `dir` to the closest multiple of `radians`."
  [dir radians]
  (if (> radians 0)
    (-> (g/heading dir)
        (/ radians)
        math/round
        (* radians)
        polar)
    dir))

(defn turn-right [[x y]]
  (v2 y (- x)))

(defn turn-left [[x y]]
  (v2 (- y) x))

(defn orientation [[px py] [qx qy] [rx ry]]
  (let [val (- (* (- qy py) (- rx qx))
               (* (- qx px) (- ry qy)))]
    (tm/sign val)))

;; TODO: use https://github.com/mourner/robust-predicates
(defn orient2d [[ax ay] [bx by] [cx cy]]
  (- (* (- ay cy) (- bx cx))
     (* (- ax cx) (- by cy))))

#?(:cljs
   (defn contains-NaN? [v]
     (some js/isNaN v)))

#?(:cljs
   (defn valid? [v]
     (not-any? js/isNaN v)))
