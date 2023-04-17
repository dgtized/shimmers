(ns shimmers.sketches.texas-fields
  (:require
   [reagent.core :as r]
   [shimmers.algorithm.lines :as lines]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.algorithm.quadtree :as saq]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.spatialtree :as spatialtree]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

;; roads need to be 2 lines, so will need to remove slices from inside
;; also roads are more interesting if they extrude outward
;; idea: pick a central circle, then attach a line at a random point tangent to curve,
;; then pick 2 points on line, left and right, and extrude lines from there

(defn make-roads [region]
  (let [{:keys [p r]} (gc/circle (rv 0.5 0.5) (* 0.1 width))
        theta (dr/random-tau)
        tangent-dir (v/polar r theta)
        tangent-pt (tm/+ p tangent-dir)
        direction (v/polar width (+ theta tm/HALF_PI))
        base-line (first (lines/clip-line (gl/line2 (tm/- tangent-pt direction) (tm/+ tangent-pt direction)) region))]
    [base-line
     (let [a (g/point-at base-line (dr/random 0.25 0.40))]
       (first (lines/clip-line (gl/line2 a (tm/+ a (tm/* tangent-dir width))) region)))
     (let [a (g/point-at base-line (dr/random 0.60 0.75))]
       (first (lines/clip-line (gl/line2 a (tm/- a (tm/* tangent-dir width))) region)))]))

(defn make-grid [cols rows]
  (for [j (butlast (tm/norm-range rows))
        i (butlast (tm/norm-range cols))]
    (rect/rect (rv i j) (tm/+ (rv i j) (gv/vec2 (/ width cols) (/ height rows))))))

;; FIXME: sometimes roads in inside of a cell, and despite the fact that the
;; base-line cuts first, it's still not always separating on subsequent cuts
(defn decompose [cell lines]
  (reduce (fn [cells line]
            (mapcat (fn [cell]
                      ;; line is scaled to handle edge cases when endpoint is on an edge but counting as inside
                      (->> (g/scale-size line 1.01)
                           (lines/cut-polygon cell)
                           ;; remove tiny slivers from output
                           (filter #(> (abs (g/area %)) 1.0))))
                    cells))
          [cell] lines))

(defn identify-zone [regions]
  (let [indexed-regions (map-indexed vector regions)]
    (fn [poly]
      (let [centroid (g/centroid poly)]
        (some (fn [[i r]] (when (g/contains-point? r centroid) i)) indexed-regions)))))

(defonce defo (debug/state))

(defn debug-info [cell]
  (reset! defo (dissoc (merge {:cell cell
                               :convex (poly-detect/convex? cell)}
                              (meta cell))
                       :on-click)))

(defn separate-with-roads [region grid roads]
  (let [zone-id (identify-zone (decompose region roads))]
    (mapcat (fn [cell]
              (if (some (fn [line] (g/intersect-line cell line)) roads)
                (for [[i poly] (map-indexed vector (decompose cell roads))]
                  (with-meta poly
                    {:fill (csvg/hsl (mod (* i tm/PHI) 1.0) 0.5 0.5 0.3)
                     :zone (zone-id poly)
                     :combine (< (g/area poly) (* 1.0 (g/area cell)))}))
                [(with-meta cell {:zone (zone-id cell)})]))
            grid)))

(defn build-tree [grid]
  (reduce (fn [qt cell]
            (if cell
              (saq/add-point qt (g/centroid cell) cell)
              qt))
          (spatialtree/quadtree 0 0 width height)
          grid))

(defn replace-shape [qt shape shape']
  (-> qt
      (g/delete-point (g/centroid shape))
      (or qt)
      (saq/add-point (g/centroid shape') shape')))

(defn mark-error [shape error]
  (vary-meta shape assoc
             :error error
             :stroke "red"
             :stroke-width 2.0))

;; not finding the longest coincident edge yet
;; FIXME: prefer attaching to polygon that stays convex, or at least add up all coincident edges
(defn find-closest [tree shape radius]
  (->> (spatialtree/select-with-circle tree (g/centroid shape) radius)
       (remove #{shape})
       (filter (fn [s] (= (:zone (meta s)) (:zone (meta shape)))))
       (apply max-key
              (fn [adj]
                (when adj
                  (if-let [segments (seq (map :segment (lines/coincident-edges shape adj)))]
                    (let [[p q] (apply max-key (fn [[p q]] (g/dist-squared p q)) segments)]
                      (g/dist-squared p q))
                    0))))))

(defn search-radius [shape]
  (apply + (:size (g/bounds shape))))

;; iterate on quadtree shapes until nothing to combine without an error
(defn join-grid [region quadtree]
  (loop [qt quadtree]
    (if-let [shape (some (fn [s] (when (let [{:keys [combine error]} (meta s)]
                                        (and combine (not error)))
                                  s))
                         (sort-by (fn [s] (abs (g/area s)))
                                  (spatialtree/select-with-shape qt region)))]
      (recur
       (if-let [closest (find-closest qt shape (search-radius shape))]
         (if-let [joined (lines/join-polygons shape closest)]
           (-> qt
               (g/delete-point (g/centroid closest))
               (replace-shape shape (with-meta joined (dissoc (meta shape) :combine))))
           (replace-shape qt shape
                          (mark-error shape [:unable-to-join closest])))
         (replace-shape qt shape
                        (mark-error shape [:no-closest]))))
      (spatialtree/select-with-shape qt region))))

(defn grid+roads [region]
  {:region region
   :grid (make-grid 8 6)
   :roads (make-roads region)})

(defn landscape [state]
  (let [{:keys [grid roads region]} (:landscape @state)
        separated-grid (separate-with-roads region grid roads)
        quadtree (build-tree separated-grid)
        closest-links
        (vec (mapcat (fn [shape]
                       (if-let [closest (find-closest quadtree shape (search-radius shape))]
                         [(gc/circle (g/centroid shape) 1.0)
                          (gl/line2 (g/centroid shape)
                                    (g/centroid closest))]
                         [(gc/circle (g/centroid shape) 3.0)]))
                     (filter (comp :combine meta) separated-grid)))

        joined-grid (vec (join-grid region quadtree))]
    [(csvg/group {:stroke "#339"}
       (for [cell joined-grid]
         (-> cell
             (vary-meta assoc :on-click #(debug-info cell))
             (vary-meta dissoc :error :combine :zone))))
     (csvg/group {:stroke "#333" :stroke-width 2} roads)
     (csvg/group {:stroke "#555"}
       (when (get-in @state [:show :closest])
         closest-links))]))

(defn scene [state]
  (reset! defo {})
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.5}
    (landscape state)))

(defn ui-controls [state]
  [:div
   (ctrl/checkbox state "Show Closest" [:show :closest])
   (debug/display defo)])

(defn page []
  (let [state (r/atom {:show {:closest true}
                       :landscape (grid+roads (rect/rect 0 0 width height))})]
    (view-sketch/page-for (partial scene state)
                          :texas-fields
                          (partial ui-controls state))))

(sketch/definition texas-fields
  {:created-at "2022-04-24"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount page "sketch-host"))
