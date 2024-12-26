(ns shimmers.sketches.degree-of-connectivity
  (:require
   [clojure.math :as math]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.graph :as graph]
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
                         (g/unmap-point bounds (gv/vec2 1.0 1.0)))
        weighted-points
        (map (fn [pos] (vary-meta pos assoc :weight (dr/pareto 0.1 1.66)))
             points)

        edges
        (for [[p q] (cs/all-pairs weighted-points)
              :let [pw (:weight (meta p))
                    qw (:weight (meta q))
                    dist (g/dist p q)
                    pd (/ dist max-dist)]
              :when (and (< pd 0.25)
                         (< (dr/random) (* 2.0 (+ pw qw))))]
          [p q])

        g (graph/adjacent-peers edges)
        cliques (graph/cliques g (keys g))]
    (println (count cliques))
    {:points points
     :edges (for [edge edges
                  :let [[p q] edge]]
              (vary-meta edge assoc
                         :weight
                         (reduce (fn [acc clique]
                                   (+ acc (* (count clique) (if (and (contains? clique p)
                                                                     (contains? clique q))
                                                              1 0))))
                                 0 cliques)))}))

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
    (concat
     (for [p (:points g)]
       (gc/circle p 1.5))
     (for [edge (:edges g)]
       (let [[p q] edge
             weight (:weight (meta edge))
             pw (:weight (meta p))
             qw (:weight (meta q))
             r (dr/random 4.0 12.0)
             d (dr/random 0.2)
             n (tm/clamp (int (* (dr/random 0.85 1.25) weight)) 0 64)
             width (tm/clamp (* 0.75 (math/log10 weight)) 0.01 2.5)
             conn (fn []
                    (vary-meta (connection p q
                                           (tm/clamp (* r pw) 2 18) (tm/clamp (* r qw) 2 18)
                                           (tm/clamp (* d pw) -0.35 0.35)
                                           (tm/clamp (* d qw) -0.35 0.35))
                               assoc :stroke-width width))]
         (csvg/group {}
           (into [] (repeatedly n conn))))))))

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
