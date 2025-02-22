(ns shimmers.sketches.ordered
  (:require
   [clojure.math :as math]
   [clojure.math.combinatorics :as mc]
   [shimmers.algorithm.lines :as lines]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.math.geometry.polygon :as poly]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce defo (debug/state {}))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))
(def max-depth 6)

(defn closest-vertex-to-line [shape line]
  (apply min-key
         (partial poly/dist-to-closest-point line)
         (g/vertices shape)))

(defn furthest-vertex-to-line [shape line]
  (apply max-key
         (partial poly/dist-to-closest-point line)
         (g/vertices shape)))

(defn pick-side [shape sides last-cut]
  (for [[side prob] sides
        :let [side-heading (g/heading side)]]
    [side
     (* (cond (and last-cut
                   (> (abs (eq/cos-similarity (v/polar (g/heading last-cut)) (v/polar side-heading))) 0.95))
              0.1
              (and (> (* 3 (g/width shape)) (g/height shape))
                   (< (sm/radial-distance (* 0.25 eq/TAU) (g/heading side)) (* 0.25 eq/TAU)))
              1.5
              (and (> (* 3 (g/height shape)) (g/width shape))
                   (< (sm/radial-distance (* 0.0 eq/TAU) (g/heading side)) (* 0.25 eq/TAU)))
              1.5
              :else 1.0)
        prob)]))

(defn cuts [polygon side offsets]
  (let [lower (closest-vertex-to-line polygon side)
        upper (furthest-vertex-to-line polygon side)
        theta (g/heading side)
        len (max (g/width polygon) (g/height polygon))
        {[lp lq] :points} (gl/line2 (v/-polar lower len theta)
                                    (v/+polar lower len theta))
        {[up uq] :points} (gl/line2 (v/-polar upper len theta)
                                    (v/+polar upper len theta))]
    (for [pct offsets]
      (gl/line2 (tm/mix lp up pct)
                (tm/mix lq uq pct)))))

(defn similar-side [polygon {[a b] :points}]
  (let [side-direction (tm/- b a)]
    (for [[p q] (g/edges polygon)
          :let [radial-d (abs (eq/cos-similarity (tm/- q p) side-direction))]]
      [(gl/line2 p q)
       (if (> radial-d 0.95)
         1.0
         0.1)])))

(defn edge-displaced
  "Displace a polygon along the axis of one of it's edges.

  Displacement distance is a multiple of the length of the selected edge."
  ([bounds side polygon] (edge-displaced bounds side polygon 10))
  ([bounds side polygon attempts]
   (let [axis (dr/weighted (similar-side polygon side))
         max-displace (min (* 0.5 (g/width bounds)) (* 0.5 (g/height bounds)))
         dist (max 1.0 (min (tm/mag axis) max-displace))
         folds (if (< (/ dist max-displace) 0.25)
                 (dr/rand-nth [2 3 4])
                 (dr/rand-nth [5 6 8]))
         direction (dr/weighted {1 4 -1 1})
         displace (v/polar (* direction (/ dist folds))
                           (g/heading axis))
         shape (g/translate polygon displace)]
     (cond (zero? attempts)
           polygon
           (collide/bounded? (g/scale-size bounds 0.99) shape)
           shape
           :else
           (recur bounds side polygon (dec attempts))))))

;; TODO: odd/even? striping displacement?
(defn perturb [bounds side perturb-rate depth terminal-stripe? [i n-cuts] polygon]
  (let [translated-poly
        (if (dr/chance
             (if (and (< depth 1) (< 0 i (dec n-cuts)))
               (* perturb-rate 1.5)
               perturb-rate))
          (edge-displaced bounds side polygon)
          polygon)]
    [(if (terminal-stripe? i)
       (+ depth max-depth)
       depth)
     (vary-meta translated-poly assoc
                :stroke-width
                (/ 1.3 (inc (dr/random depth))))]))

(defn extrusion [shape bounds children y-off]
  (let [extruded (vary-meta (g/translate shape y-off)
                            assoc :stroke-width 0.5)]
    (if (collide/bounded? bounds extruded)
      (concat children
              [extruded]
              (mapv (fn [a b]
                      (vary-meta (gl/line2 a b) assoc :stroke-width 0.5))
                    (g/vertices shape)
                    (g/vertices extruded)))
      children)))

(defn recurse-shapes
  [{:keys [bounds p-extrusion sides] :as rules} shape last-side depth]
  (if (or (> depth max-depth)
          (< (g/area shape) (* 0.00005 width height))
          (some (fn [[p q]] (< (g/dist p q) 3))
                (g/edges shape)))
    [shape]
    (let [side (dr/weighted (pick-side shape sides last-side))
          n-cuts (dr/weighted {0 (max 0 (* (- depth 2) 3))
                               1 5
                               2 5
                               3 4
                               4 3
                               5 2
                               6 1
                               7 1
                               8 1})
          ;; FIXME: how can offsets mirror outward from center cuts for powers >1
          power (dr/weighted {1 2
                              1.25 2
                              tm/PHI 4
                              2 2
                              3 1})
          offsets (map (fn [x] (math/pow x power))
                       (tm/norm-range n-cuts))
          terminal-stripe? (if (and (> n-cuts 1) (dr/chance 0.5))
                             (dr/weighted {odd? 2 even? 1})
                             (constantly nil))
          perturb-rate (* (dr/rand-nth [0.02 0.05 0.1])
                          (/ 1 (math/pow 2 depth)))
          ;; FIXME: inset-polygon causes too many errors dwonstream
          layers #{}
          shape' (if (contains? layers depth)
                   (->> (poly-detect/inset-polygon shape 3)
                        poly-detect/split-self-intersection
                        (apply max-key g/area))
                   shape)
          y-off (tm/* (gv/vec2 0 (* 1.6 (- 8 depth)))
                      (if (dr/chance 0.5) 1 -1))]
      (mapcat (fn [s i]
                (let [[p-depth p-shape] (perturb bounds side perturb-rate depth terminal-stripe? [i n-cuts] s)
                      children (recurse-shapes rules p-shape side (inc p-depth))]
                  (if (dr/chance (* depth p-extrusion))
                    (extrusion p-shape bounds children y-off)
                    children)))
              (lines/slice-polygons [shape'] (cuts shape' side offsets))
              (range)))))

(defn extend [p q len]
  (let [theta (g/heading (tm/- q p))]
    (gl/line2 (v/-polar p len theta)
              (v/+polar q len theta))))

(defn outside-shapes [bounds shape]
  (let [len (max (g/width bounds) (g/height bounds))]
    (->> (reduce (fn [shapes [p q]]
                   (mapcat (fn [s]
                             (->>
                              (extend p q len)
                              (lines/cut-polygon s)
                              (filter (fn [s] (> (g/area s) 1)))))
                           shapes))
                 [bounds]
                 (g/edges shape))
         (remove (fn [s] (g/contains-point? shape (g/centroid s)))))))

(defn rectangle []
  (let [[pw ph] (dr/weighted {[0.5 0.75] 1
                              [0.33 0.66] 1})]
    (-> (rect/rect 0 0 (* pw width) (* ph height))
        g/center
        (g/rotate (* eq/TAU (dr/rand-nth [(/ 1 8) (/ 1 6) (/ 5 8) (/ 5 6)])))
        (g/translate (rv 0.5 0.5)))))

(defn parallelogram [region]
  (let [[pw ph] (dr/rand-nth (mc/cartesian-product
                              [(/ 1 tm/PHI) 0.66 0.75 0.8]
                              [(/ 1 tm/PHI) 0.66 0.75 0.8]))
        rect (-> (rect/rect 0 0 (* pw (g/width region)) (* ph (g/height region)))
                 (g/center (g/centroid region)))
        [a b c d] (g/vertices rect)
        displace (gv/vec2 (* (dr/random-sign) (g/width rect) 0.5 pw) 0)
        [a b c d] (if (dr/chance 0.5)
                    [(g/translate a displace)
                     (g/translate b displace)
                     c d]
                    [a b
                     (g/translate c displace)
                     (g/translate d displace)])]
    (-> (gp/polygon2 a b c d)
        (g/rotate (* eq/TAU (dr/weighted {0 2 (/ 1 8) 1 (/ 1 6) 1})))
        (g/translate (rv 0.5 0.5)))))

(defn n-gon [n]
  (-> (poly/regular-n-gon n (* 0.49 height))
      (g/rotate (* eq/TAU (dr/rand-nth [0 (/ 1 8) (/ 1 6) (/ 5 8) (/ 5 6)])))
      (g/translate (rv 0.5 0.5))))

(defn sides-distribution [shapes]
  (for [[p q] (mapcat g/edges shapes)]
    [(gl/line2 p q) 1.0]))

(defn bounded-shape-in-region [bounds region]
  (let [s (dr/weighted
           [[region 1.0]
            [(rectangle) 2.0]
            [(parallelogram region) 2.0]
            [(n-gon 5) 2.0]
            [(n-gon 6) 3.0]
            [(n-gon 8) 1.0]])
        shape (first (gu/fit-all-into-bounds region [s]))
        side-shapes (distinct (filter some? [bounds
                                             region
                                             (when (= s region) (n-gon 6))
                                             shape]))]
    [shape side-shapes]))

(defn generate-shapes [{:keys [bounds] :as rules} shape]
  (let [inner (recurse-shapes rules shape nil 0)
        outer (map (fn [s] (vary-meta s assoc :stroke-width 0.15))
                   (outside-shapes bounds shape))]
    [inner outer]))

;; TODO: adjust generate-shapes to fill some of the surrounding regions
;; however, gu/fit-all-into-bounds only works if bounds is an aabb and not a polygon
;; so I need to implement a fit-all-into-polygon method.
(defn shapes [{:keys [bounds] :as rules}]
  (let [[shape side-shapes]
        (bounded-shape-in-region bounds
                                 (if (dr/chance 0.5) bounds (g/scale-size bounds 0.9)))
        [inner outer]
        (generate-shapes (assoc rules :sides (sides-distribution side-shapes)) shape)
        approach (dr/weighted [[identity 1.0]
                               [empty 2.0]])
        split-shapes (concat inner (approach outer))]
    (swap! defo assoc :rules rules)
    (swap! defo update :shapes conj (count split-shapes))
    #_(swap! defo assoc
             :shape shape
             :sides side-shapes)
    ;; FIXME: mostly if the shape appears empty it looks like it's from multiple
    ;; copies of the origin shape, and not because it didn't split enough, so
    ;; maybe a bug in split generation or the cut-polygon routine?
    (if (< 150 (count split-shapes) 1000)
      split-shapes
      (recur rules))))

(defn ruleset []
  {:bounds (csvg/screen width height)
   :p-extrusion (dr/weighted {0.0 3.0
                              0.01 2.0
                              0.015 1.0
                              0.02 1.0})})

(defn scene [{:keys [scene-id rules]}]
  (reset! defo {:shapes []})
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "none"}
    (shapes rules)))

(sketch/definition ordered
  {:created-at "2023-02-24"
   :type :svg
   :tags #{}}
  (ctrl/mount (usvg/page
               (assoc sketch-args
                      :rules (ruleset)
                      :explanation (fn [] [debug/display defo]))
               scene)))
