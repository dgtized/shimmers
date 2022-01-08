(ns shimmers.sketches.triangle-gradient
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.core :as g]
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.math.core :as tm]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.rect :as rect]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes []
  (let [template (g/center (gt/triangle2 (gv/vec2 0 0) (gv/vec2 40 40) (gv/vec2 30 5)))
        generate (fn []
                   (-> template
                       (g/rotate (dr/random 0 tm/TWO_PI))
                       (g/translate (gv/vec2 (* width (dr/gaussian 0.33 0.15))
                                             (dr/random (* 0.2 height) (* 0.8 height))))))]
    (gu/fit-all-into-bounds (rect/rect 0 0 width height)
                            (repeatedly 1000 generate))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 0.8}
            (shapes)))

(sketch/definition triangle-gradient
  {:created-at "2022-01-07"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :triangle-gradient)
              "sketch-host"))
