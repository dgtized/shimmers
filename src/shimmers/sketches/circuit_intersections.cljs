(ns shimmers.sketches.circuit-intersections
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.color.core :as col]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 600)
(def height 800)
(defn r [x y]
  (gv/vec2 (* width x) (* height y)))

(defn color [i]
  (col/as-css (col/hsla (mod (* i tm/PHI) 1.0) 0.5 0.5 1.0)))

(defn placement [wires size center]
  (let [n (count wires)
        nrange (tm/norm-range (dec n))
        width (+ (* size n) (* (dec n) 0.5 size))
        half (* 0.5 width)]
    (vec (for [offset nrange]
           (+ (- (* offset width) half) center)))))

(defn scene []
  (let [sources (range 8)
        destinations (range 4)
        src (mapv #(r % 0.9) (placement sources 0.01 0.5))
        dst (mapv #(r 0.1 %) (placement destinations 0.01 0.5))
        connections (->> (fn [] [(dr/rand-nth sources)
                                (dr/rand-nth destinations)])
                         (repeatedly 8))]
    (csvg/svg {:width width :height height :stroke "black"}
              (for [[a b] connections
                    :let [p (nth src a)
                          q (nth dst b)
                          isec (gv/vec2 (:x p) (:y q))]]
                (svg/group {}
                           (svg/circle isec 4.0)
                           (svg/line p isec)
                           (svg/line isec q))))))

(defn page []
  [:div (scene)])

(sketch/definition circuit-intersections
  {:created-at "2021-11-08"
   :type :svg
   :tags #{:static}}
  (ctrl/mount page "canvas-host"))
