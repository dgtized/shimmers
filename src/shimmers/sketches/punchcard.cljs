(ns shimmers.sketches.punchcard
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; Inspired by: https://twitter.com/akacastor/status/1480657994241036289/photo/1
(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes [bounds]
  (let [columns (g/subdivide bounds {:cols 16 :rows (dr/weighted {1 3 2 1})})]
    (apply concat
           (for [column columns
                 :let [grid (g/subdivide (g/scale-size column 0.9) {:cols 8 :rows 64})]]
             (->> grid
                  (dr/random-sample (dr/random 0.2 0.8))
                  (map (fn [b] (g/translate (rect/rect 4) (tm/+ (:p b) (gv/vec2 1 0)))))
                  (into [(g/scale-size column 0.95)]))))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.8}
    (shapes (g/scale-size (csvg/screen width height) 0.95))))

(sketch/definition punchcard
  {:created-at "2022-01-22"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/static-page scene :punchcard)))
