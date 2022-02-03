(ns shimmers.sketches.planetary-arcs
  (:require
   [loom.alg :as la]
   [loom.graph :as lg]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg :refer [ISVGConvert]]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [thi.ng.geom.utils :as gu]))

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

(defn clockwise-intercept [center bounds]
  (let [midpoints (map (fn [[p q]] (tm/mix p q 0.5)) (g/edges bounds))]
    (apply min (map #(g/heading (tm/- % center)) midpoints))))

(comment (clockwise-intercept (gv/vec2 -5 11) (rect/rect 10)))

(defn ellipse-arc [center a b intercept dt]
  (for [t (range intercept (+ intercept eq/TAU) dt)]
    (tm/+ center (gv/vec2 (* a (Math/cos t))
                          (* b (Math/sin t))))))

(comment (ellipse (gv/vec2 1 0 ) 10 10 0.1))

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

(defn random-arc-points [arcs]
  (g/point-at (dr/weighted-by (fn [{:keys [points]}] (gu/arc-length points)) arcs)
              (dr/random)))

(defn polar-graph [arcs n]
  (->> (repeatedly #(random-arc-points arcs))
       (take (* n 1.5))
       (minimum-separation (* 0.05 height))
       (take n)
       cs/all-pairs
       poly-detect/edges->graph
       la/prim-mst))

(comment (polar-graph (rect/rect 0 0 width height) 5))

(defn mst-graph [bounds n]
  (let [k (inc (int (Math/sqrt n)))
        points (->> (g/subdivide bounds {:rows k :cols k})
                    dr/shuffle
                    (mapv (fn [rect] (tm/+ (g/centroid rect) (dr/jitter (* 0.05 height)))))
                    (minimum-separation (* 0.05 height))
                    (take n))]
    (la/prim-mst (poly-detect/edges->graph (cs/all-pairs points)))))

(defn neighbors-with-distance [g n]
  (->> (lg/successors g n)
       (sort-by #(lg/weight g n %))
       (map (fn [p] [p (lg/weight g n p)]))))

(defn max-radius-per-point [bounds graph]
  (into {}
        (for [p (lg/nodes graph)
              :let [neighbors (neighbors-with-distance graph p)
                    [_ dist] (first neighbors)
                    r (min dist (* 0.975 (g/dist p (g/closest-point bounds p))))]]
          [p r])))

;; TODO: maybe expand circles until they bump a neighbor?
(defn planet-graph []
  (let [bounds (rect/rect 0 0 width height)
        n (dr/weighted {11 2
                        17 2
                        23 2
                        31 2
                        41 1
                        63 1})
        center (g/point-at (g/scale-size (g/bounding-circle bounds) 1.1) (dr/random 0.3 0.4))
        arcs (polar-arcs center
                         bounds
                         (dr/random 1 1.15)
                         (dr/random 1.15 1.3)
                         0.1
                         (dr/var-range (max 11 (int (/ n 3)))))
        graph (polar-graph arcs n)
        _ (swap! defo assoc :planets (count (lg/nodes graph))
                 :arcs (count arcs))
        max-radius (max-radius-per-point bounds graph)
        circles (map (fn [p] (let [neighbors (neighbors-with-distance graph p)
                                  [_ dist] (first neighbors)
                                  r (min (* 0.475 dist)
                                         (* 0.975 (g/dist p (g/closest-point bounds p))))]
                              (gc/circle p r)))
                     (lg/nodes graph))]
    (concat (mapcat (fn [{:keys [p r]}]
                      (let [neighbors (neighbors-with-distance graph p)
                            density (if (dr/chance 0.1)
                                      (Math/ceil (* (/ r (* 0.04 height))
                                                    (dr/rand-nth [7 11 13])))
                                      (cond (< r (* 0.05 height)) 3
                                            (< r (* 0.1 height)) 4
                                            :else (dr/random-int 5 8)))]
                        (planet p r angle-gen
                                (mapv (fn [[n _]] [(g/heading (tm/- n p)) density]) neighbors))))
                    circles)
            #_(map (fn [c] (with-meta c {:stroke-width 0.5 :stroke "green"})) circles)
            #_(map (fn [[p r]] (with-meta (gc/circle p r) {:stroke-width 0.5 :stroke "green"})) max-radius)
            ;; TODO: show arcs in a way that doesn't clip bodies and looks like rotation sweeps?
            ;; randomize dasharray?
            #_(map #(with-meta % {:stroke-dasharray "0.1% 0.5% 0.1% 0.5% 0.1% 11%"
                                  :stroke-dashoffset (* 0.01 height (dr/rand-nth [-6 -4 0 2 8]))}) arcs)
            (map (fn [[p q]]
                   (let [pr (get max-radius p)
                         qr (get max-radius q)
                         between (tm/mix p q (/ pr (+ pr qr)))]
                     (gc/circle between 1.0)))
                 (lg/edges graph))
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

(defn page []
  [:div
   [:div.canvas-frame [scene]]
   [:div.explanation
    [:div.flexcols
     [:div (view-sketch/generate :planetary-arcs)]
     [debug/display defo]]]])

(sketch/definition planetary-arcs
  {:created-at "2022-01-31"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount page "sketch-host"))
