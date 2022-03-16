(ns shimmers.sketches.constellations
  (:require
   [loom.alg :as la]
   [loom.attr :as lga]
   [loom.graph :as lg]
   [reagent-keybindings.keyboard :as kb]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.svg-export :as svg-export]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.graph :as graph]
   [shimmers.math.points :as points]
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

;; TODO: alternate rendering with wider arcs?
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

(defn ellipse [center a b dt]
  (-> (for [t (range 0 eq/TAU dt)]
        (gv/vec2 (* a (Math/cos t))
                 (* b (Math/sin t))))
      gp/polygon2
      (g/translate center)))

;; FIXME: every once and a while it doesn't fill the space with this?
;; like only the first couple arcs generate?
(defn polar-arcs [center bounds a b dt range-offsets]
  (let [v (sort-by #(g/dist center %) (g/vertices bounds))
        r-min (g/dist center (first v))
        r-max (g/dist center (last v))
        inside? (fn [p] (g/contains-point? (g/scale-size bounds 0.9) p))]
    (for [rt range-offsets
          :let [r (tm/mix* r-min r-max rt)
                scaled-dt (max (* (- 1 rt) dt) 0.01)
                points (->> (ellipse center (* a r) (* b r) scaled-dt)
                            :points
                            (drop-while (complement inside?))
                            (take-while inside?))]
          :when (> (count points) 2)]
      (gl/linestrip2 points))))

(defn generate-arc-points [arcs]
  (let [arc-weights (cs/mapping (fn [{:keys [points]}] (gu/arc-length points)) arcs)]
    (repeatedly #(g/point-at (dr/weighted arc-weights) (dr/random)))))

(defn polar-graph [arcs n]
  (->> (generate-arc-points arcs)
       (take (* n 1.5))
       (points/minimum-separation (* 0.05 height))
       (take n)
       graph/points->graph
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

(defn add-neighbor-to-lonely
  "Add a few cycles to MST graph by connecting a `percent` sampling of
  single out-degree nodes to a nearby, unconnected, planar neighbor.

  Ensures new neighbor edges are > `min-angle` from an existing edge."
  [graph percent min-angle]
  (letfn [(possibly-add-edge [g p]
            (let [angles (map (fn [n] (g/heading (tm/- n p))) (lg/successors g p))
                  big-angle? (fn [q] (not-any? #(tm/delta= % (g/heading (tm/- q p)) min-angle) angles))]
              (if-let [candidate (->> (lg/nodes g)
                                      (remove (fn [q] (or (= p q) (lg/has-edge? g p q))))
                                      (sort-by (partial g/dist-squared p))
                                      (some (fn [q]
                                              ;; this is only checking if edge with lonely node is large
                                              ;; and not if the angle is large enough for the candidate
                                              (when (and (big-angle? q)
                                                         (graph/planar-edge? g p q)
                                                         (not (graph/edge-clips-node-radius? g p q)))
                                                q))))]
                (lg/add-edges g [p candidate (g/dist p candidate)])
                g)))]
    (->> (lg/nodes graph)
         (filter #(= 1 (lg/out-degree graph %))) ;; "lonely" nodes only
         (dr/random-sample percent)
         (reduce possibly-add-edge graph))))

(defn specify-density [graph]
  (reduce (fn [g p]
            (let [r (lga/attr g p :radius)
                  rfactor (Math/ceil (/ r (* 0.03 height)))
                  density (cond (dr/chance 0.08)
                                (* rfactor (dr/rand-nth [7 11 13]))
                                (< r (* 0.025 height)) (dr/random-int 2 5)
                                (< r (* 0.060 height)) (dr/random-int 3 6)
                                (dr/chance 0.5) (dr/random-int 6 9)
                                :else
                                (Math/ceil (* (/ rfactor (lg/out-degree g p))
                                              (dr/random-int 5 8))))]
              (lga/add-attr g p :density density)))
          graph (lg/nodes graph)))

(defn generate-arcs [bounds n]
  (let [galaxy-center (-> (g/bounding-circle bounds)
                          (g/scale-size 1.2)
                          (g/point-at (+ (dr/random 0.07 0.18) (/ (dr/random-int 4) 4))))]
    (polar-arcs galaxy-center
                bounds
                (dr/random 1 1.15)
                (dr/random 1.15 1.3)
                0.1
                (if (dr/chance 0.5)
                  (dr/density-range 0.008 0.08)
                  (dr/var-range n)))))

;; TODO: assign high density as an attribute to k elements before building?
(defn plot-planets [graph]
  (mapcat (fn [p]
            (let [r (lga/attr graph p :radius)
                  density (lga/attr graph p :density)]
              (planet p r angle-gen
                      (mapv (fn [n] [(g/heading (tm/- n p)) density])
                            (lg/successors graph p)))))
          (lg/nodes graph)))

;; midpoint can still fall inside of a third planet if the edge clips the radius
(defn plot-midpoints [graph]
  (mapcat (fn [[p q]]
            (let [pr (lga/attr graph p :radius)
                  qr (lga/attr graph q :radius)
                  mid-ratio (/ pr (+ pr qr))
                  between (tm/mix p q mid-ratio)
                  m 1.05
                  theta (g/heading (tm/- q p))
                  scale (/ (g/dist p q) (+ pr qr))]
              (if (and (> (g/dist between p) (* m pr))
                       (> (g/dist between q) (* m qr)))
                (if (> scale 2.0)
                  [(g/translate (g/rotate (g/as-polygon (gc/circle 1.66) 6) theta) between)
                   (gc/circle (tm/mix p q (* mid-ratio 0.96)) 0.33)
                   (gc/circle (tm/mix p q (* mid-ratio 1.04)) 0.33)]
                  [(gc/circle between 1.0)])
                [])))
          (graph/unique-edges (lg/edges graph))))

(defn filtered-arcs [graph arcs]
  (let [overlap-graph
        (fn [point]
          (some (fn [p]
                  (let [c (gc/circle p (* 1.1 (lga/attr graph p :radius)))]
                    (when (g/contains-point? c point) p)))
                (lg/nodes graph)))]
    (mapcat (fn [arc]
              (let [parts (partition-by overlap-graph (:points arc))]
                (->> parts
                     (remove (fn [points] (or (< (count points) 4)
                                             (overlap-graph (first points)))))
                     (mapv gl/linestrip2))))
            (filter (fn [{:keys [points]}] (> (count points) 2)) arcs))))

;; FIXME: sometimes arc segments are looping back to beginning somehow?
;; though I think that is happening from the arc generation not the plot
;; TODO: skip over edges in the graph?
(defn plot-arcs [graph arcs]
  (let [spacing (/ height 150)]
    (apply concat
           (for [arc (filtered-arcs graph arcs)
                 :let [length (/ (tm/mag arc) spacing)
                       range (tm/norm-range (tm/ceil length))
                       beat (dr/rand-nth [23 29 31 41])
                       offset (dr/random-int (tm/ceil (/ beat 2)))]]
             (for [[i [a b]] (map-indexed vector (partition 2 1 range))
                   :let [major (= 0 (mod (+ i offset) beat))
                         leading (= 0 (mod (- (+ i offset) 2) beat))
                         trailing (= 0 (mod (+ (+ i offset) 2) beat))]
                   :when (or leading major trailing)]
               (if major
                 (gl/linestrip2 (g/point-at arc a)
                                (g/point-at arc b))
                 (gl/linestrip2 (g/point-at arc (tm/mix* a b 0.25))
                                (g/point-at arc (tm/mix* a b 0.75)))))))))

(defn planet-graph [bounds]
  (let [n (dr/weighted {11 2
                        17 3
                        19 4
                        23 5
                        29 3
                        31 2
                        41 1
                        63 1})
        arcs (generate-arcs bounds (max 11 (int (/ n 3))))
        graph (-> (polar-graph arcs n)
                  (radius-per-point bounds) ;; need radius for adding neighbors
                  (add-neighbor-to-lonely 0.4 0.66)
                  (radius-per-point bounds) ;; recalculate after adding neighbors
                  grow-planets
                  (shrink-planets (/ n 120))
                  specify-density)]
    (swap! defo assoc
           :planets (count (lg/nodes graph))
           :arcs (count arcs))
    (concat (plot-planets graph)
            (plot-midpoints graph)
            (plot-arcs graph arcs)
            #_(map (fn [p] (with-meta (gc/circle p (lga/attr graph p :radius))
                            {:stroke-width 0.5 :stroke "green"})) (lg/nodes graph))
            #_(map (fn [p] (with-meta (gc/circle p (lga/attr graph p :max-radius))
                            {:stroke-width 0.5 :stroke "green"})) (lg/nodes graph))
            #_(map (fn [[a b]] (gl/line2 a b)) (lg/edges graph))
            )))

(defn arcs-test [bounds]
  (generate-arcs bounds (dr/random-int 9 23)))

(defn planet-pair [_]
  (concat (planet (rv 0.25 0.5) (* height 0.3) angle-gen
                  [[(* 0.0 eq/TAU) 7] [(* 0.25 eq/TAU) 7]])
          (planet (rv 0.75 0.5) (* height 0.3) angle-gen
                  [[(* 0.33 eq/TAU) 7] [(* 0.5 eq/TAU) 7] [(* 0.66 eq/TAU) 11]])))

(def modes {:constellations planet-graph
            :arcs-test arcs-test
            :planet-test planet-pair})

(defonce ui-state (ctrl/state {:mode :constellations}))

(defn scene []
  (let [bounds (rect/rect 0 0 width height)
        scene-fn (get modes (:mode @ui-state))]
    (reset! defo {})
    (csvg/svg {:id "scene"
               :width width
               :height height
               :stroke "black"
               :fill "none"
               :stroke-width 0.8}
              (apply list (scene-fn bounds)))))

(defn ui-controls []
  [:div
   [ctrl/change-mode ui-state (keys modes)]
   [kb/kb-action "alt-s" #(svg-export/download "scene" "constellations")]
   [debug/display defo]])

(sketch/definition constellations
  {:created-at "2022-01-31"
   :type :svg
   :tags #{:deterministic}}
  (-> scene
      (view-sketch/page-for :constellations ui-controls)
      (ctrl/mount "sketch-host")))
