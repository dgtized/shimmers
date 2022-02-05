(ns shimmers.sketches.constellations
  (:require
   [loom.alg :as la]
   [loom.attr :as lga]
   [loom.graph :as lg]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.intersection :as isec]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg :refer [ISVGConvert]]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defonce defo (debug/state))

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
               #_{:stroke (if (= sweep 1) "blue" "red")})))

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

;; might ocassionally be overlapping more than one input line on triplets+?
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
  (cond (dr/chance 0.05)
        (let [rel- (dr/random (* 0.25 radial0) (* 0.85 radial0))
              ;; avoid completing the circle, but might be biasing?
              r1 (min (- eq/TAU rel-) radial1)
              rel+ (dr/random (* 0.25 r1) (* 0.85 r1))]
          [(- angle rel-) (+ rel+ rel-)])
        (dr/chance 0.5)
        [angle (- (dr/random (* 0.25 radial0) (* 0.85 radial0)))]
        :else
        [angle (dr/random (* 0.25 radial1) (* 0.85 radial1))]))

(defn minimum-separation
  "Only keep `points` if closest pair distance is greather than `threshold`."
  [threshold points]
  (reduce (fn [accepted p]
            (if (every? #(> (g/dist p %) threshold) accepted)
              (conj accepted p)
              accepted))
          [] points))

(defn ellipse [center a b dt]
  (-> (for [t (range 0 eq/TAU dt)]
        (gv/vec2 (* a (Math/cos t))
                 (* b (Math/sin t))))
      gp/polygon2
      (g/translate center)))

;; FIXME: sometimes this looks like it's closing the loop?
(defn polar-arcs [center bounds a b dt range-offsets]
  (let [v (sort-by #(g/dist center %) (g/vertices bounds))
        r-min (g/dist center (first v))
        r-max (g/dist center (last v))]
    (for [rt range-offsets
          :let [r (tm/mix* r-min r-max rt)
                scaled-dt (max (* (- 1 rt) dt) 0.01)
                points (->> (ellipse center (* a r) (* b r) scaled-dt)
                            :points
                            (filter (fn [p] (g/contains-point? (g/scale-size bounds 0.9) p))))]
          :when (> (count points) 2)]
      (gl/linestrip2 points))))

(defn generate-arc-points [arcs]
  (let [arc-weights (cs/mapping (fn [{:keys [points]}] (gu/arc-length points)) arcs)]
    (repeatedly #(g/point-at (dr/weighted arc-weights) (dr/random)))))

(defn polar-graph [arcs n]
  (->> (generate-arc-points arcs)
       (take (* n 1.5))
       (minimum-separation (* 0.05 height))
       (take n)
       cs/all-pairs
       poly-detect/edges->graph
       la/prim-mst))

(defn neighbors-with-distance [g n]
  (->> (lg/successors g n)
       (sort-by #(lg/weight g n %))
       (map (fn [p] [p (lg/weight g n p)]))))

(defn radius-per-point [graph bounds]
  (reduce (fn [g p]
            (let [neighbors (neighbors-with-distance graph p)
                  [_ dist] (first neighbors)
                  max-radius (min dist (* 0.975 (g/dist p (g/closest-point bounds p))))
                  radius (min (* 0.475 dist) max-radius)]
              (-> g
                  (lga/add-attr p :max-radius max-radius)
                  (lga/add-attr p :radius radius))))
          graph
          (lg/nodes graph)))

(defn grow-radius-of-planet [graph planet]
  (let [{:keys [radius max-radius]} (lga/attrs graph planet)
        neighbors (neighbors-with-distance graph planet)
        p-radius (apply min (map (fn [[neighbor distance]]
                                   (- distance (lga/attr graph neighbor :radius)))
                                 neighbors))
        legal-radius (min p-radius max-radius)]
    (if (and (> legal-radius radius) (> (- legal-radius radius) (* 0.1 radius)))
      (-> graph
          (lga/add-attr planet :orig-radius radius)
          (lga/add-attr planet
                        :radius (tm/mix* radius legal-radius (dr/random 0.25 0.5))))
      graph)))

(defn grow-planets [graph]
  (reduce grow-radius-of-planet graph (lg/nodes graph)))

;; Maybe should consolidate growth/shrink and calculate radius stats first to
;; try and juke the stats towards variance. That or compare against neighbors?
(defn shrink-planets [graph likelyhood]
  (reduce (fn [g planet]
            (let [radius (lga/attr g planet :radius)]
              (if (dr/chance likelyhood)
                (-> g
                    (lga/add-attr planet :orig-radius radius)
                    (lga/add-attr planet :radius (* (dr/random 0.5 0.8) radius)))
                g)))
          graph (lg/nodes graph)))

(defn planar-edge?
  "Check if edge `p`-`q` in `graph` is planar.

  The edge is planar if all intersections with existing edges are at `p` or `q`."
  [graph p q]
  (every? (fn [[a b]]
            (let [isec (isec/segment-intersect [p q] [a b])]
              (or (and isec (or (tm/delta= isec p) (tm/delta= isec q)))
                  true)))
          (lg/edges graph)))

(defn add-neighbor-to-lonely
  "Add a few cycles to MST graph by connecting a `percent` sampling of
  single out-degree nodes to a nearby, unconnected, planar neighbor.

  Ensures new neighbor edges are > `min-angle` from an existing edge."
  [graph percent min-angle]
  (let [lonely (filter #(= 1 (lg/out-degree graph %)) (lg/nodes graph))]
    (reduce (fn [g p]
              (let [angles (map (fn [n] (g/heading (tm/- n p))) (lg/successors g p))
                    big-angle? (fn [q] (not-any? #(tm/delta= % (g/heading (tm/- q p)) min-angle) angles))]
                (if-let [candidate (->> (lg/nodes g)
                                        (remove (fn [q] (or (= p q) (lg/has-edge? g p q))))
                                        (sort-by (partial g/dist-squared p))
                                        (some (fn [q]
                                                ;; this is only checking if edge with lonely node is large
                                                ;; and not if the angle is large enough for the candidate
                                                (when (and (big-angle? q) (planar-edge? g p q))
                                                  q))))]
                  (lg/add-edges g [p candidate (g/dist p candidate)])
                  g)))
            graph
            (dr/random-sample percent lonely))))

;; TODO: assign high density as an attribute to k elements before building?
(defn generate-planets [graph]
  (mapcat (fn [p]
            (let [r (lga/attr graph p :radius)
                  density (cond (dr/chance 0.08)
                                (Math/ceil (* (/ r (* 0.03 height))
                                              (dr/rand-nth [7 11 13])))
                                (< r (* 0.025 height)) (dr/random-int 2 4)
                                (< r (* 0.050 height)) (dr/random-int 3 5)
                                :else (dr/random-int 5 8))]
              (planet p r angle-gen
                      (mapv (fn [n] [(g/heading (tm/- n p)) density])
                            (lg/successors graph p)))))
          (lg/nodes graph)))

(defn plot-midpoints [graph]
  (map (fn [[p q]]
         (let [pr (lga/attr graph p :radius)
               qr (lga/attr graph q :radius)
               between (tm/mix p q (/ pr (+ pr qr)))]
           (gc/circle between 1.0)))
       (lg/edges graph)))

(defn planet-graph []
  (let [bounds (rect/rect 0 0 width height)
        n (dr/weighted {11 2
                        17 2
                        23 2
                        31 2
                        41 1
                        63 1})
        galaxy-center (-> (g/bounding-circle bounds)
                          (g/scale-size 1.2)
                          (g/point-at (+ (dr/random 0.07 0.18) (/ (dr/random-int 4) 4))))
        arcs (polar-arcs galaxy-center
                         bounds
                         (dr/random 1 1.15)
                         (dr/random 1.15 1.3)
                         0.1
                         (dr/var-range (max 11 (int (/ n 3)))))
        graph (-> (polar-graph arcs n)
                  (add-neighbor-to-lonely 0.4 0.66)
                  (radius-per-point bounds)
                  grow-planets
                  (shrink-planets (/ n 120)))]
    (swap! defo assoc
           :planets (count (lg/nodes graph))
           :arcs (count arcs))
    (concat (generate-planets graph)
            (plot-midpoints graph)

            #_(map (fn [p] (with-meta (gc/circle p (lga/attr graph p :radius))
                            {:stroke-width 0.5 :stroke "green"})) (lg/nodes graph))
            #_(map (fn [p] (with-meta (gc/circle p (lga/attr graph p :max-radius))
                            {:stroke-width 0.5 :stroke "green"})) (lg/nodes graph))
            ;; TODO: show arcs in a way that doesn't clip bodies and looks like rotation sweeps?
            ;; randomize dasharray?
            (map (fn [arc]
                   (->> {:stroke-dasharray "0.1% 0.5% 0.1% 0.5% 0.1% 13%"
                         :stroke-dashoffset (* 0.01 height (dr/rand-nth [-6 -4 0 2 8]))}
                        (with-meta arc)))
                 arcs)
            #_(map (fn [[a b]] (gl/line2 a b)) (lg/edges graph))
            )))

(defn arcs-test []
  (polar-arcs (gv/vec2 -30 -20) (rect/rect 0 0 width height)
              1.05 1.15 0.1
              (dr/var-range 16)
              ))

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
             :stroke-width 0.8}
            (apply list (planet-graph))))

(sketch/definition constellations
  {:created-at "2022-01-31"
   :type :svg
   :tags #{:deterministic}}
  (-> scene
      (view-sketch/with-controls :constellations
        (partial debug/display defo))
      (ctrl/mount "sketch-host")))
