(ns shimmers.sketches.deeper-squares
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn inset-rect [rect scale]
  (let [[a _ c _] (gp/inset-polygon (g/vertices rect) scale)]
    (rect/rect a c)))

(defn deepen [{op :p [width height] :size :as outer}]
  (let [{ip :p [w h] :size :as inner} (inset-rect outer (dr/random -16 -4))]
    (g/translate inner (tm/- (gv/vec2 (* (dr/random 0.2 0.8) (- width w))
                                      (* (dr/random 0.2 0.8) (- height h)))
                             (tm/- ip op)))))

(defn shapes []
  (svg/group {} (take 20 (iterate deepen (rect/rect (rv 0.2 0.1) (rv 0.8 0.9))))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 0.5}
            (shapes)))

(sketch/definition deeper-squares
  {:created-at "2022-01-04"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount (view-sketch/page-for scene :deeper-squares)
              "sketch-host"))
