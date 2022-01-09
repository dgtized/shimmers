(ns shimmers.sketches.glyphs
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn glyph []
  [(gl/linestrip2 (repeatedly (dr/random-int 6 8) #(gv/vec2 (dr/random 1 5) (dr/random 1 7))))
   (gl/linestrip2 (repeatedly (dr/random-int 1 4) #(gv/vec2 (dr/random 1 5) (dr/random 1 7))))])

(defn shapes []
  (let [wm 10
        hm 10
        rows 14
        columns 12
        dw (/ (- width wm wm) columns)
        dh (/ (- height hm hm) rows)]
    (for [i (range wm (- width wm) dw)
          j (range hm (- height hm) dh)]
      (svg/group {:transform (str "translate(" i "," j ")")}
                 (gu/fit-all-into-bounds (rect/rect 0 0 (* 0.9 dw) (* 0.9 dh))
                                         (glyph))))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 0.5}
            (shapes)))

(sketch/definition glyphs
  {:created-at "2022-01-08"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :glyphs)
              "sketch-host"))
