(ns shimmers.sketches.texas-fields
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.color :as color]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.spatialtree :as spatialtree]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn make-roads []
  [(gl/line2 (rv 0 (dr/random 0.2 0.8)) (rv 1 (dr/random 0.2 0.8)))
   (gl/line2 (rv (dr/random 0.2 0.8) 0) (rv (dr/random 0.2 0.8) 1))])

(defn make-grid []
  (for [j (butlast (tm/norm-range 15))
        i (butlast (tm/norm-range 20))]
    (rect/rect (rv i j) (tm/+ (rv i j) (gv/vec2 (/ width 20) (/ height 15))))))

(defn decompose [cell lines]
  (reduce (fn [cells line]
            (mapcat (fn [cell] (lines/cut-polygon cell line)) cells))
          [cell] lines))

(defn separate-with-roads [grid roads]
  (mapcat (fn [cell]
            (if (some (fn [line] (g/intersect-line cell line)) roads)
              (let [group (random-uuid)]
                (for [[i poly] (map-indexed vector (decompose cell roads))]
                  (with-meta poly
                    {:fill (color/css-hsl (mod (* i tm/PHI) 1.0) 0.5 0.5 0.3)
                     :combine (< (g/area poly) (* 0.9 (g/area cell)))
                     :group group})))
              [cell]))
          grid))

(defn build-tree [grid]
  (reduce (fn [qt cell]
            (if cell
              (g/add-point qt (g/centroid cell) cell)
              qt))
          (spatialtree/quadtree 0 0 width height)
          grid))

;; not finding the longest coincident edge yet
(defn find-closest [tree shape radius]
  (->> (spatialtree/select-with-circle tree (g/centroid shape) radius)
       (remove #{shape})
       (remove (fn [s] (= (:group (meta s)) (:group (meta shape)))))
       (apply max-key
              (fn [adj]
                (if-let [segments (seq (map :segment (lines/coincident-edges shape adj)))]
                  (let [[p q] (apply max-key (fn [[p q]] (g/dist-squared p q)) segments)]
                    (g/dist-squared p q))
                  0)))))

(defn landscape []
  (let [roads (make-roads)
        grid (make-grid)
        separated-grid (separate-with-roads grid roads)
        quadtree (build-tree separated-grid)
        radius (let [x (first grid)]
                 (* 1.2 (max (g/width x) (g/height x))))
        closest-links
        (mapcat (fn [shape]
                  [(gc/circle (g/centroid shape) 1.0)
                   (gl/line2 (g/centroid shape)
                             (g/centroid (find-closest quadtree shape radius)))])
                (filter (comp :combine meta) separated-grid))]
    (concat (for [shape separated-grid]
              (if (:combine (meta shape))
                (vary-meta shape dissoc :combine :group)
                shape))
            roads
            (svg/group {:stroke "black"} closest-links)
            )))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.5}
            (apply list (landscape))))

(sketch/definition texas-fields
  {:created-at "2022-04-24"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount (view-sketch/page-for scene :texas-fields)
              "sketch-host"))
