(ns shimmers.sketches.degree-of-connectivity
  (:require
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; basic concept: random graph but with weights on each edge based on logic like
;; how many times it reduces planarity of the graph, so some are heavily shaded
;; and others are just faint edges.

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn graph [bounds]
  (let [points (rp/poisson-disc-sampling (g/scale-size bounds 0.95) 50)
        max-dist (g/dist (g/unmap-point bounds (gv/vec2 0 0))
                         (g/unmap-point bounds (gv/vec2 1.0 1.0)))]
    {:points points
     :edges (for [[p q] (cs/all-pairs points)
                  :let [dist (g/dist p q)
                        pd (/ dist max-dist)]
                  :when (and (< pd 0.175)
                             (< (dr/random) 0.5))]
              [p q])}))

(defn connection [p q r d]
  (let [p' (rp/confusion-disk p r)
        q' (rp/confusion-disk q r)]
    (gl/line2 (tm/mix p' q' (dr/random (- d) d))
              (tm/mix p' q' (- 1.0 (dr/random (- d) d))))))

(defn shapes [bounds]
  (let [g (graph bounds)]
    (concat (for [p (:points g)]
              (gc/circle p 1.5))
            (for [[p q] (:edges g)]
              (let [r (dr/random 2.0 10.0)
                    d (dr/random 0.075)]
                (csvg/group {}
                  (into [] (repeatedly (int (* 24 (dr/pareto 0.125 1.16)))
                                       #(connection p q r d)))))))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.5}
    (shapes (rect/rect 0 0 width height))))

(sketch/definition degree-of-connectivity
  {:created-at "2024-12-19"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
