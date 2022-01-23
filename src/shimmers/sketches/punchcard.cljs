(ns shimmers.sketches.punchcard
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.core :as g]
   [shimmers.math.deterministic-random :as dr]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes [bounds]
  (let [columns (g/subdivide bounds {:cols 16 :rows 1})]
    (->> columns
         (map (fn [c] (g/subdivide (g/scale-size c 0.9) {:cols 8 :rows 64})))
         (map #(dr/random-sample (dr/random 0.2 0.8) %))
         (mapcat (fn [c] (map (fn [b] (g/translate (rect/rect 4) (:p b))) c))))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 1.0}
            (shapes (g/scale-size (rect/rect 0 0 width height) 0.95))))

(sketch/definition punchcard
  {:created-at "2022-01-22"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :punchcard)
              "sketch-host"))
