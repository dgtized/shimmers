(ns shimmers.sketches.radial-wings
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn rp [r theta]
  (v/polar (* r 0.5 height) theta))

(defn shapes []
  (->> (for [[a b] (->> (dr/density-range 0.05 0.1 true)
                        (partition 2 1)
                        (dr/random-sample 0.8))
             :let [[ra rb] (map (partial + 0.1) [a b])]]
         (gp/polygon2 [(rp 0 0)
                       (rp ra (* a tm/TWO_PI))
                       (rp rb (* b tm/TWO_PI))]))
       (mapv #(g/translate % (rv 0.5 0.5)))))

;; FIXME: handle large gaps and overlapping lines
(defn scene []
  (csvg/timed
   (csvg/svg {:width width
              :height height
              :stroke "black"
              :fill "white"
              :stroke-width 0.5}
     (shapes))))

(sketch/definition radial-wings
  {:created-at "2021-11-15"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount (view-sketch/page-for scene :radial-wings)
              "sketch-host"))
