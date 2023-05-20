(ns shimmers.sketches.window-glimpses
  (:require
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.palette :as palette]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.sketches.radial-mosaic :as radial-mosaic]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.utils.intersect :as isec]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn distinct-shape [scale existing shape]
  (if (some (fn [s] (collide/overlaps? (g/scale-size shape scale) s)) existing)
    existing
    (conj existing shape)))

(defn gen-box [{[width height] :size :as bounds} existing]
  (let [w (dr/random-int (* 0.04 width) (int (* 0.44 width)))
        h (dr/random-int (* 0.04 height) (int (* 0.44 height)))
        poly (g/as-polygon
              (rect/rect (dr/random-int 0 (- width w))
                         (dr/random-int 0 (- height h))
                         w h))
        box (if (dr/chance 0.2)
              (geometry/rotate-around-centroid poly (dr/gaussian 0.0 0.08))
              poly)]
    (if (collide/bounded? bounds box)
      (distinct-shape 1.12 existing box)
      existing)))

(defn gen-circle [{[width height] :size} existing]
  (let [r (dr/random-int (int (* 0.05 (min width height)))
                         (int (* 0.44 (min width height))))
        circle (gc/circle (dr/random-int r (- width r))
                          (dr/random-int r (- height r))
                          r)]
    (distinct-shape 1.05 existing circle)))

(defn generate [bounds f n]
  (->> []
       (iterate (partial f bounds))
       (take-while (fn [s] (< (count s) n)))
       last))

(defn clipped [shape shapes]
  (for [s shapes
        :when (collide/overlaps? shape s)]
    (with-meta (g/clip-with shape s) (meta s))))

(defn intersecting-points
  "Finds all intersection points along a line through a set of edges sorted by
  distance from `rp`."
  [rp rq edges]
  (->> edges
       (sequence
        (comp
         (map (fn [[p q]] (isec/intersect-line2-line2? rp rq p q)))
         (filter (fn [isec]
                   (when (get isec :p) (= :intersect (get isec :type)))))
         (map (fn [isec] (let [p (get isec :p)
                              d (g/dist-squared rp p)]
                          [p d])))))
       (sort-by second)
       (map first)))

;; FIXME: this only works for convex shapes
(defn separate
  "Clips a line with shapes, such that it returns the set of lines that are not
  contained within the shapes. "
  [{[rp rq] :points :as line} shapes]
  (let [polygons (filter (partial collide/overlaps? line) shapes)]
    (if (empty? polygons)
      [line]
      (map gl/line2
           (partition 2 2 (concat [rp]
                                  (->> polygons
                                       (mapcat g/edges)
                                       (intersecting-points rp rq))
                                  [rq]))))))

(defn mass-vary [shapes field value]
  (map (fn [s] (vary-meta s assoc field value))
       shapes))

(defn swap-triangles [circles n]
  (let [[triangles circles'] (split-at n (dr/shuffle circles))]
    (concat (map (fn [c] (g/as-polygon (triangle/inscribed-equilateral c (dr/random-tau))))
                 triangles)
            (map (fn [c] (g/as-polygon c 64)) circles'))))

(defn dashed-line [line pattern]
  (->> pattern
       cycle
       (map (fn [x] (/ 1.0 (* 8 x))))
       (reductions +)
       (take-while (fn [l] (<= l 1.0)))
       (partition 2 2)
       (map (fn [[a b]] (gl/line2 (g/point-at line a) (g/point-at line b))))))

(comment (dashed-line (gl/line2 0 0 0 10) [1 2 3]))

(defn shapes [bounds palette]
  (let [[ocolor & palette] (dr/shuffle palette)
        theta0 (dr/random-tau)
        theta1 (dr/gaussian (+ theta0 (/ eq/TAU 4)) (/ eq/TAU 8))
        boxes (generate bounds gen-box 20)
        lines (clip/hatch-rectangle bounds (* width 0.08) theta0)
        circles (swap-triangles (generate bounds gen-circle 16) (dr/random-int 4))
        [as bs] (if (dr/chance 0.66)
                  [boxes circles]
                  [circles boxes])
        bs (map (fn [s] (vary-meta s assoc :fill (dr/rand-nth palette))) bs)
        clipped-bs (mapcat (fn [a] (clipped a bs)) as)
        inner-lines
        (mapcat (fn [line]
                  (let [subset (mapcat (fn [shape] (lines/clip-line line shape)) clipped-bs)]
                    (if (dr/chance 0.15)
                      (mapcat (fn [segment] (dashed-line segment [2 3 5])) subset)
                      subset)))
                (clip/hatch-rectangle bounds (* width 0.03) theta1))]
    (concat
     (-> as
         (mass-vary :stroke-width 1.5)
         (mass-vary :fill ocolor))
     clipped-bs
     (mass-vary (mapcat (fn [line] (separate line as)) lines)
                :stroke-width 0.5)
     (mass-vary inner-lines :stroke-width 0.5))))

;; TODO: curate palettes for this sketch -- dark inner is often weird, and need
;; higher contrast between color pairs..
;; TODO: color a specific strip between two of the inner stripe lines across all
;; shapes it's clipped with.
(defn scene [palette]
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 1.0}
    (shapes (rect/rect 0 0 width height) palette)))

(defn pick-palette []
  (-> (concat radial-mosaic/palettes
              [] ;; no palette
              [["#ffeedd" "#ddeeff"]])
      dr/rand-nth))

(defn page []
  (let [palette (pick-palette)]
    (fn []
      [:div
       [:div.canvas-frame [scene palette]]
       [:div.contained
        [palette/as-svg {:class "center" :height 10} palette]
        [:p.center (view-sketch/generate :window-glimpses)]]])))

(sketch/definition window-glimpses
  {:created-at "2023-05-18"
   :tags #{}
   :type :svg}
  (ctrl/mount page))
