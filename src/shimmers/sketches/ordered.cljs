(ns shimmers.sketches.ordered
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.polygon :as poly]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce defo (debug/state {}))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn closest-vertex-to-line [shape line]
  (apply min-key
         (partial poly/dist-to-closest-point line)
         (g/vertices shape)))

(defn furthest-vertex-to-line [shape line]
  (apply max-key
         (partial poly/dist-to-closest-point line)
         (g/vertices shape)))

(defn pick-side [shape sides last-cut]
  (let [angle (when last-cut (g/heading last-cut))]
    (dr/weighted
     (for [[side prob] sides
           :let [side-heading (g/heading side)]]
       [side
        (* (cond (and last-cut (< (sm/radial-distance angle side-heading) 0.05))
                 0.1
                 (and (> (* 3 (g/width shape)) (g/height shape))
                      (< (sm/radial-distance (* 0.25 eq/TAU) (g/heading side)) (* 0.25 eq/TAU)))
                 1.5
                 (and (> (* 3 (g/height shape)) (g/width shape))
                      (< (sm/radial-distance (* 0.0 eq/TAU) (g/heading side)) (* 0.25 eq/TAU)))
                 1.5
                 :else 1)
           prob)]))))

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

(defn edge-displacement
  "Displace a polygon along the axis of one of it's edges.

  Displacement distance is a multiple of the length of the selected edge."
  [polygon]
  (let [axis (apply gl/line2 (dr/rand-nth (g/edges polygon)))
        d (min (tm/mag axis) (* 0.5 width) (* 0.5 height))]
    (v/polar (/ d (dr/rand-nth [2 3 4 5 6]))
             (g/heading axis))))

(defn slice [polygon lines depth]
  (reduce (fn [polygons line]
            (mapcat (fn [poly]
                      (->> line
                           (lines/cut-polygon poly)
                           (filter #(> (count (:points %)) 0))
                           (map (fn [cut-poly]
                                  (let [area (g/area cut-poly)
                                        region (* width height)
                                        disp
                                        (if (and (> depth 1)
                                                 (< (* 0.005 region) area (* 0.015 region))
                                                 (dr/chance 0.02))
                                          (edge-displacement cut-poly)
                                          (gv/vec2))]
                                    (vary-meta (g/translate cut-poly disp) assoc
                                               :stroke-width
                                               (/ 1.3 (inc (dr/random depth)))))))))
                    polygons))
          [polygon] lines))

(defn recurse-shapes [sides shape last-side depth]
  (if (or (> depth 6)
          (< (g/area shape) (* 0.00005 width height))
          (some (fn [[p q]] (< (g/dist p q) 3))
                (g/edges shape)))
    [shape]
    (let [side (pick-side shape sides last-side)
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
                              tm/PHI 4
                              2 2
                              3 1})
          offsets (map (fn [x] (Math/pow x power))
                       (tm/norm-range n-cuts))
          stripes? (and (> n-cuts 1) (odd? n-cuts) (dr/chance 0.5))
          ;; FIXME: inset-polygon causes too many errors dwonstream
          layers #{}
          shape' (if (contains? layers depth)
                   (->> (poly-detect/inset-polygon shape 3)
                        poly-detect/split-self-intersection
                        (apply max-key g/area))
                   shape)]
      (mapcat (fn [s i]
                (if  (and stripes? (odd? i))
                  [s]
                  (recurse-shapes sides s side (inc depth))))
              (slice shape' (cuts shape' side offsets) depth)
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

(defn n-gon [n]
  (-> (poly/regular-n-gon n (* 0.49 height))
      (g/rotate (* eq/TAU (dr/rand-nth [(/ 1 8) (/ 1 6) (/ 5 8) (/ 5 6)])))
      (g/translate (rv 0.5 0.5))))

(defn sides-distribution [shapes]
  (for [[p q] (mapcat g/edges shapes)]
    [(gl/line2 p q) 1.0]))

(defn bounded-shape-in-region [bounds region]
  (let [s (dr/weighted
           [[region 1.0]
            [(rectangle) 2.0]
            [(n-gon 5) 1.0]
            [(n-gon 6) 3.0]
            [(n-gon 8) 1.0]])
        shape (first (gu/fit-all-into-bounds region [s]))
        side-shapes (distinct (filter some? [bounds
                                             region
                                             (when (= s bounds) (n-gon 6))
                                             shape]))]
    [shape side-shapes]))

(defn generate-shapes [bounds shape side-shapes]
  (let [inner (recurse-shapes (sides-distribution side-shapes) shape nil 0)
        outer (map (fn [s] (vary-meta s assoc :stroke-width 0.15))
                   (outside-shapes bounds shape))]
    [inner outer]))

;; TODO: adjust generate-shapes to fill some of the surrounding regions
;; however, gu/fit-all-into-bounds only works if bounds is an aabb and not a polygon
;; so I need to implement a fit-all-into-polygon method.
(defn shapes []
  (let [bounds (rect/rect 0 0 width height)
        [shape side-shapes] (bounded-shape-in-region bounds bounds)
        [inner outer] (generate-shapes bounds shape side-shapes)
        approach (dr/weighted [[identity 1.0]
                               [empty 1.0]])
        split-shapes (concat inner (approach outer))]
    (swap! defo update :shapes conj (count split-shapes))
    #_(swap! defo assoc
             :shape shape
             :sides side-shapes)
    ;; FIXME: mostly if the shape appears empty it looks like it's from multiple
    ;; copies of the origin shape, and not because it didn't split enough, so
    ;; maybe a bug in split generation or the cut-polygon routine?
    (if (< 150 (count split-shapes) 1000)
      split-shapes
      (recur))))

(defn scene []
  (reset! defo {:shapes []})
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"}
    (shapes)))

(defn ui-controls []
  [:div
   (debug/display defo)])

(sketch/definition ordered
  {:created-at "2023-02-24"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :ordered ui-controls)
              "sketch-host"))
