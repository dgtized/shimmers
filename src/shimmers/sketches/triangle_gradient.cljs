(ns shimmers.sketches.triangle-gradient
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)

(defn shapes []
  (let [template (g/center (gt/triangle2 [0 0] [40 40] [30 5]))
        dir (if (dr/chance 0.3)
              (fn [x] (- 1.0 x))
              (fn [x] x))
        generate (fn []
                   (let [x (Math/pow (dr/random) 0.4)
                         y (dr/random 0.2 0.8)]
                     (-> template
                         (g/scale-size (+ 0.15 (tm/clamp01 (Math/pow (dir x) 1.4))))
                         (g/rotate (dr/random 0 tm/TWO_PI))
                         (g/translate (gv/vec2 (* width x) (* height y))))))]
    (gu/fit-all-into-bounds (rect/rect 0 0 width height)
                            (repeatedly 4000 generate))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 0.5}
            (shapes)))

(sketch/definition triangle-gradient
  {:created-at "2022-01-07"
   :type :svg
   :tags #{:static :deterministic}}
  (ctrl/mount (view-sketch/page-for scene :triangle-gradient)
              "sketch-host"))
