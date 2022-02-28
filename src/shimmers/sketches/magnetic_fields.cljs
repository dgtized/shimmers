(ns shimmers.sketches.magnetic-fields
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [shimmers.math.deterministic-random :as dr]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn closest [dipoles pos]
  (apply min-key (fn [dipole] (g/dist-squared pos (:p dipole))) dipoles))

(defn line [bounds dipoles]
  (let [start (g/random-point-inside (g/scale-size bounds 0.9))
        step (fn [pos] (let [{:keys [p strength]} (closest dipoles pos)
                            [x y] (tm/- pos p)
                            b (tm/cross (gv/vec3 x y 0) (gv/vec3 0 0 strength))
                            a (gv/vec2 (:x b) (:y b))]
                        (tm/+ pos (tm/* a 0.025))))]
    (gl/linestrip2 (take 50 (iterate step start)))))

(defn shapes []
  (let [bounds (rect/rect 0 0 width height)
        dipoles (repeatedly 3 (fn [] {:p (g/random-point-inside (g/scale-size bounds 0.5))
                                     :strength (if (dr/chance 0.5) (dr/random 2 6) (- (dr/random 2 6)))}))]
    [(svg/group {} (map (fn [{:keys [p strength]}] (gc/circle p (Math/abs strength))) dipoles))
     (svg/group {} (repeatedly 50 #(line bounds dipoles)))]))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.5}
            (apply list (shapes))))

(sketch/definition magnetic-fields
  {:created-at "2022-02-27"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :magnetic-fields)
              "sketch-host"))
