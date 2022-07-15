(ns shimmers.sketches.glyphs
  (:require
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.utils.subdiv :as gsd]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)

(defn smooth-line [points]
  (->> points
       (gsd/subdivide-closed (:chaikin gsd/schemes))
       (gsd/subdivide-closed (:chaikin gsd/schemes))
       cs/midsection
       (cs/sandwich points)))

(defn glyph []
  (let [shapes (dr/weighted {0 6
                             1 3
                             2 2
                             3 1})]
    (concat
     [(let [pts (repeatedly (dr/random-int 4 8) #(gv/vec2 (dr/random 1 5) (dr/random 1 6)))]
        (gl/linestrip2 (if (dr/chance 0.8)
                         (smooth-line pts)
                         pts)))]
     (for [_ (range 1 shapes)]
       (case (dr/random-int 3)
         0 (gc/circle (gv/vec2 (dr/random 2 4) (dr/random 1 6)) (dr/random 0.5 2))
         1 (gl/linestrip2 (repeatedly (dr/random-int 2 4) #(gv/vec2 (dr/random 1 5) (dr/random 1 4))))
         2 (gl/linestrip2 (repeatedly 2 #(gv/vec2 (dr/random 1 5) (dr/random 3 7)))))))))

(defn shapes []
  (let [wm 10
        hm 10
        rows 20
        columns 30
        dw (/ (- width wm wm) columns)
        dh (/ (- height hm hm) rows)]
    (for [i (range wm (- width wm) dw)
          j (range hm (- height hm) dh)]
      (csvg/group {:transform (str "translate(" i "," j ")")}
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
