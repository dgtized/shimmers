(ns shimmers.sketches.planetary-arcs
  (:require
   [loom.alg :as la]
   [loom.graph :as lg]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg :refer [ISVGConvert]]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn radial-angle
  "Calculate angle distance between `lower` and `upper` angles.

  If `lower` bound is greater than `upper`, prefer the distance the long way
  around the circle."
  [lower upper]
  (let [delta (sm/radial-distance lower upper)]
    (if (< lower upper)
      delta
      (- eq/TAU delta))))

(defrecord RelativeArc [start end radius large-arc sweep]
  ISVGConvert
  (as-svg [_ _opts]
    (csvg/path [[:M start] [:A [radius radius] 0 large-arc sweep end]]
               {:stroke (if (= sweep 1) "blue" "red")})))

(defn relative-arc
  "Calculate arc flags for an SVG path from a start-angle to a relative theta.

  FIXME: Doesn't handle completing a circle if |dtheta| >= 𝜏."
  [p r start-angle dtheta]
  (let [end-angle (+ start-angle dtheta)
        start (tm/+ p (v/polar r start-angle))
        end (tm/+ p (v/polar r end-angle))
        large-arc (if (<= (Math/abs (- end-angle start-angle)) Math/PI) 0 1)
        sweep (if (> dtheta 0) 1 0)]
    (->RelativeArc start end r large-arc sweep)))

;; occasional bug with offset arcs somehow with correct radius but wrong center?
(defn planet [p radius gen-angle inputs]
  (let [total-arcs (reduce + (map second inputs))
        all-ranges (dr/shuffle (rest (dr/var-range (+ total-arcs 1))))
        ordered-inputs (sort-by first inputs)]
    (->> (mapcat
          (fn [[angle0 angle angle1] ranges]
            (let [ranges (sort ranges)
                  radial0 (radial-angle angle0 angle)
                  radial1 (radial-angle angle angle1)]
              (into [(gl/line2 (tm/+ p (v/polar (* radius (first ranges)) angle))
                               (tm/+ p (v/polar (* radius (last ranges)) angle)))]
                    (for [arc ranges
                          :let [[a b] (gen-angle arc angle radial0 radial1)]]
                      (relative-arc p (* radius arc) a b)))))
          (cs/triplet-cycle (map first ordered-inputs))
          (cs/partition-chunks (map second ordered-inputs) all-ranges))
         (into [(gc/circle p 1)]))))

(defn angle-gen [_ angle radial0 radial1]
  (cond (dr/chance 0.2)
        (let [rel- (dr/random (* 0.25 radial0) (* 0.85 radial0))
              rel+ (dr/random (* 0.25 radial1) (* 0.85 radial1))]
          [(- angle rel-) (+ rel+ rel-)])
        (dr/chance 0.5)
        [angle (- (dr/random (* 0.25 radial0) (* 0.85 radial0)))]
        :else
        [angle (dr/random (* 0.25 radial1) (* 0.85 radial1))]))

(defn mst-graph [bounds n]
  (let [points (mapv (fn [rect] (g/random-point-inside rect))
                     (g/subdivide bounds {:num (Math/ceil (Math/sqrt n))}))]
    (la/prim-mst (poly-detect/edges->graph (cs/all-pairs points)))))

(defn neighbors-with-distance [g n]
  (->> (lg/successors g n)
       (sort-by #(lg/weight g n %))
       (map (fn [p] [p (lg/weight g n p)]))))

(defn planet-graph []
  (let [bounds (rect/rect 0 0 width height)
        graph (mst-graph (g/scale-size bounds 0.85) 6)]
    (concat (mapcat (fn [p]
                      (let [neighbors (neighbors-with-distance graph p)
                            [_ dist] (first neighbors)
                            min-dist (min dist (g/dist p (g/closest-point bounds p)))]
                        (planet p (* 0.49 min-dist) angle-gen
                                (mapv (fn [[n _]] [(g/heading (tm/- n p)) 3]) neighbors))))
                    (lg/nodes graph))
            #_(map (fn [[a b]] (gl/line2 a b)) (lg/edges graph))
            )))

(defn planet-pair []
  (concat (planet (rv 0.25 0.5) (* height 0.3) angle-gen
                  [[(* 0.0 eq/TAU) 7] [(* 0.25 eq/TAU) 7]])
          (planet (rv 0.75 0.5) (* height 0.3) angle-gen
                  [[(* 0.33 eq/TAU) 7] [(* 0.5 eq/TAU) 7] [(* 0.66 eq/TAU) 11]])))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 0.5}
            (apply list (planet-graph))))

(sketch/definition planetary-arcs
  {:created-at "2022-01-31"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :planetary-arcs)
              "sketch-host"))
