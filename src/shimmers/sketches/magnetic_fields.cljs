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

(defn line-step [dipoles]
  (fn [pos]
    (let [forces (for [{:keys [p strength]} dipoles
                       :let [[x y] (tm/- pos p)
                             b (tm/cross (gv/vec3 x y 0) (gv/vec3 0 0 strength))
                             a (gv/vec2 (:x b) (:y b))]]
                   (tm/* a (/ 200 (+ (g/dist-squared pos p) 1e-5))))]
      (reduce tm/+ pos forces))))

(defn line [bounds dipoles]
  (let [start (g/random-point-inside (g/scale-size bounds 0.9))]
    (gl/linestrip2 (take 50 (iterate (line-step dipoles) start)))))

(defn shapes []
  (let [bounds (rect/rect 0 0 width height)
        dipoles (repeatedly (dr/random-int 2 5)
                            (fn [] {:p (g/random-point-inside (g/scale-size bounds 0.5))
                                   :strength (if (dr/chance 0.5) (dr/random 2 6) (- (dr/random 2 6)))}))]
    [(svg/group {} (for [{:keys [p strength]} dipoles]
                     (with-meta (gc/circle p (Math/abs strength))
                       {:fill (if (> strength 0) "none" "black")})))
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
