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
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.color.core :as col]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.types :refer [Circle2]]
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
        poly (rect/rect (dr/random-int 0 (- width w))
                        (dr/random-int 0 (- height h))
                        w h)
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

(defn smooth-poly [s]
  (if (instance? Circle2 s)
    (g/as-polygon s 64)
    (g/as-polygon s)))

(defn clipped [shape shapes]
  (for [s shapes
        :when (collide/overlaps? shape s)]
    (with-meta
      (if (collide/bounded? s shape)
        shape
        (g/clip-with (smooth-poly shape) (smooth-poly s)))
      (meta s))))

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
;; FIXME: adjust point intersection logic for circles instead of always using edges
(defn separate
  "Clips a line with shapes, such that it returns the set of lines that are not
  contained within the shapes. "
  [{[rp rq] :points :as line} shapes]
  (let [polygons (filter (partial collide/overlaps? line) shapes)]
    (if (empty? polygons)
      [line]
      (->>
       (concat [rp]
               (->> polygons
                    (mapcat (fn [s]
                              (if (instance? Circle2 s)
                                (g/edges s 64)
                                (g/edges s))))
                    (intersecting-points rp rq))
               [rq])
       (partition 2 2)
       (map gl/line2)))))

(defn mass-vary [shapes field value]
  (map (fn [s] (vary-meta s assoc field value))
       shapes))

(defn swap-triangles [circles n]
  (let [[triangles circles'] (split-at n (dr/shuffle circles))]
    (concat (map (fn [c] (triangle/inscribed-equilateral c (dr/random-tau)))
                 triangles)
            circles')))

(defn dashed-line [line pattern]
  (->> pattern
       cycle
       (map (fn [x] (/ 1.0 (* 8 x))))
       (reductions +)
       (take-while (fn [l] (<= l 1.0)))
       (partition 2 2)
       (map (fn [[a b]] (gl/line2 (g/point-at line a) (g/point-at line b))))))

(comment (dashed-line (gl/line2 0 0 0 10) [1 2 3]))


(defn separate-palette [palette]
  (if (seq palette)
    (let [background (dr/weighted-by (comp col/luminance col/hex->int) palette)]
      (concat [background] (remove #{background} palette)))
    palette))

(defn clip-lines-to-shapes [lines shapes]
  (mapcat
   (fn [line]
     (mapcat (fn [shape]
               (lines/clip-line line shape))
             shapes))
   lines))

;; (separate-palette (pick-palette))
;; (separate-palette (first (palette/from-urls [palette/slate-shell-red-tan-yellow])))
;; (separate-palette (first (palette/from-urls [palette/orange-maroon-blues])))
;; (separate-palette [])

(defn shapes [bounds palette]
  (let [[background & palette] (separate-palette palette)
        theta0 (dr/random-tau)
        theta1 (dr/gaussian (+ theta0 (/ eq/TAU 4)) (/ eq/TAU 8))
        theta2 (dr/gaussian (/ (+ theta0 theta1) 2) (/ eq/TAU 16))
        boxes (generate bounds gen-box 20)
        lines (clip/hatch-rectangle bounds (* width 0.08) theta0)
        circles (swap-triangles (generate bounds gen-circle 16) (dr/random-int 4))
        [as bs] (if (dr/chance 0.75)
                  [boxes circles]
                  [circles boxes])
        bs (map (fn [s] (vary-meta s assoc :fill (dr/rand-nth palette)
                                  :cross (dr/chance 0.08))) bs)
        clipped-bs (mapcat (fn [a] (clipped a bs)) as)
        inner-lines
        (mapcat (fn [line]
                  (let [subset (mapcat (fn [shape] (lines/clip-line line shape)) clipped-bs)]
                    (if (dr/chance 0.15)
                      (mapcat (fn [segment] (dashed-line segment [2 3 5])) subset)
                      subset)))
                (clip/hatch-rectangle bounds (* width 0.03) theta1))

        crossed
        (clip-lines-to-shapes
         (clip/hatch-rectangle bounds (* width 0.015) theta2)
         (filter (fn [s] (:cross (meta s))) clipped-bs))]
    [(csvg/group {:fill background
                  :stroke-width 1.5}
       as)
     (csvg/group {} clipped-bs)
     (csvg/group {:stroke-width 0.5}
       (mapcat (fn [line] (separate line as)) lines))
     (csvg/group {:stroke-width 0.5}
       inner-lines)
     (csvg/group {:stroke-width 0.125}
       crossed)]))

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
  (->> [:blue-yellow-tan-brown
        :shell-blue-yellow-grey
        :purple-shell-brown
        :shell-grey-blues
        :slate-shell-red-tan-yellow
        :slate-red-yellow-blue-brown
        :shell-grey-blues-bold ;; blues are maybe too close?
        :yellow-blue-slate-grey-red
        :red-black-yellow-grey-blue
        :orange-black-blue-shell-red
        :orange-maroon-blues
        :blues-orange-black-shell
        :yellow-slate-white-mint-red
        :black-shell-red-maroon-red
        :green-shades]
       palette/by-names
       (map :colors)
       (concat [[]] ;; no palette
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
