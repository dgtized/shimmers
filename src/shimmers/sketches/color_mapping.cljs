(ns shimmers.sketches.color-mapping
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.color :as color]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)

(defn color [seed i j]
  (let [n (dr/noise-at-point seed 0.02 (gv/vec2 i j))]
    (color/css-hsl (mod (* n tm/PHI) 1.0) 0.5 0.45 1.0)))

(defn shapes []
  (let [[cols rows] [80 60]
        w (/ width cols)
        h (/ height rows)
        seed (gv/vec2 (dr/random 100) (dr/random 100))]
    (for [j (range rows)
          i (range cols)]
      (vary-meta (rect/rect (* i w) (* j h) w h)
                 assoc :fill (color seed i j)))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "none"}
            (shapes)))

(sketch/definition color-mapping
  {:created-at "2022-05-02"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :color-mapping)
              "sketch-host"))
