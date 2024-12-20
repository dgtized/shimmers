(ns shimmers.sketches.degree-of-connectivity
  (:require
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.bezier :as bezier]
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
  (let [points (rp/poisson-disc-sampling (g/scale-size bounds 0.95)
                                         (+ 15 (* 5 (dr/random-int 5))))
        max-dist (g/dist (g/unmap-point bounds (gv/vec2 0 0))
                         (g/unmap-point bounds (gv/vec2 1.0 1.0)))]
    {:points points
     :edges (for [[p q] (cs/all-pairs
                         (map (fn [pos] (vary-meta pos assoc :w (dr/pareto 0.1 1.66)))
                              points))
                  :let [pw (:w (meta p))
                        qw (:w (meta q))
                        dist (g/dist p q)
                        pd (/ dist max-dist)]
                  :when (and (< pd 0.225)
                             (< (dr/random) (* 2.0 (+ pw qw))))]
              [p q])}))

(defn connection [p q rp rq dp dq]
  (let [p' (rp/confusion-disk p rp)
        q' (rp/confusion-disk q rq)]
    (if (dr/chance (* 0.01 (+ rp rq)))
      (let [mid (rp/confused-midpoint p' q' (* 0.01 (+ rp rq)))]
        (gl/linestrip2 (g/sample-uniform (bezier/auto-spline2 [p' mid q']) 16 true)))
      (gl/line2 (tm/mix p' q' (dr/random (- dp) dp))
                (tm/mix p' q' (- 1.0 (dr/random (- dq) dq)))))))

(defn shapes [bounds]
  (let [g (graph bounds)]
    (concat (for [p (:points g)]
              (gc/circle p 1.5))
            (for [[p q] (:edges g)]
              (let [pw (:w (meta p))
                    qw (:w (meta q))
                    r (dr/random 4.0 12.0)
                    d (dr/random 0.2)]
                (csvg/group {}
                  (into [] (repeatedly (tm/clamp (int (* 24 (+ pw qw) (dr/random 0.2 0.8))) 0 64)
                                       #(connection p q
                                                    (tm/clamp (* r pw) 2 18) (tm/clamp (* r qw) 2 18)
                                                    (tm/clamp (* d pw) -0.35 0.35)
                                                    (tm/clamp (* d qw) -0.35 0.35))))))))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.75}
    (shapes (rect/rect 0 0 width height))))

(sketch/definition degree-of-connectivity
  {:created-at "2024-12-19"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
