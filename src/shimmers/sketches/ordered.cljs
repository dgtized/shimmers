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

(defn distance-to-closest-point [shape p]
  (-> shape
      (g/closest-point p)
      (g/dist p)))

(defn closest-vertex-to-line [shape line]
  (apply min-key
         (partial distance-to-closest-point line)
         (g/vertices shape)))

(defn furthest-vertex-to-line [shape line]
  (apply max-key
         (partial distance-to-closest-point line)
         (g/vertices shape)))

(defn pick-side [sides last-cut]
  (let [angle (when last-cut (g/heading last-cut))]
    (dr/weighted
     (for [[side prob] sides]
       [side
        (* (if (and last-cut (< (sm/radial-distance angle (g/heading side)) 0.05))
             0.1
             1)
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

(defn slice [polygon lines]
  (reduce (fn [polygons line]
            (mapcat (fn [poly] (filter #(> (count (:points %)) 0)
                                      (lines/cut-polygon poly line))) polygons))
          [polygon] lines))

(defn recurse-shapes [sides shape last-side depth]
  (if (or (> depth 6)
          (< (g/area shape) 8)
          (some (fn [[p q]] (< (g/dist p q) 3))
                (g/edges shape)))
    [shape]
    (let [side (pick-side sides last-side)
          n-cuts (dr/weighted {0 (max 0 (* (- depth 2) 3))
                               1 5
                               2 5
                               3 4
                               4 3
                               5 2
                               6 1
                               7 1
                               8 1})
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
              (slice shape' (cuts shape' side offsets))
              (range)))))

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
  (for [[p q] (->> shapes
                   (filter some?)
                   (mapcat g/edges))]
    [(gl/line2 p q) 1.0]))

(defn shapes []
  (let [bounds (rect/rect 0 0 width height)
        s (dr/weighted
           [[bounds 3.0]
            [(rectangle) 2.0]
            [(n-gon 5) 1.0]
            [(n-gon 6) 3.0]
            [(n-gon 8) 1.0]])
        shape (first (gu/fit-all-into-bounds bounds [s]))
        side-shapes [bounds
                     (when (= s bounds) (n-gon 6))
                     shape]
        split-shapes (recurse-shapes (sides-distribution side-shapes) shape nil 0)]
    (swap! defo update :shapes conj (count split-shapes))
    ;; FIXME: mostly if the shape appears empty it looks like it's from multiple
    ;; copies of the origin shape, and not because it didn't split enough, so
    ;; maybe a bug in split generation or the cut-polygon routine?
    (if (< 150 (count split-shapes) 750)
      split-shapes
      (recur))))

(defn scene []
  (reset! defo {:shapes []})
  (csvg/timed
   (csvg/svg {:width width
              :height height
              :stroke "black"
              :fill "none"}
     (shapes))))

(defn ui-controls []
  [:div
   (debug/display defo)])

(sketch/definition ordered
  {:created-at "2023-02-24"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :ordered ui-controls)
              "sketch-host"))
