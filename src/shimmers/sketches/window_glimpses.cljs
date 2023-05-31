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
   [shimmers.math.geometry.arc :as arc]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.color.core :as col]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.types :refer [Circle2 Triangle2]]
   [thi.ng.geom.utils.intersect :as isec]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn distinct-shape [scale existing shape]
  (if (some (fn [s] (collide/overlaps? (g/scale-size shape scale) s)) existing)
    existing
    (conj existing shape)))

(defn parallelogram [rect upper-left angle]
  (let [[a b c d] (g/vertices rect)
        a-b (g/dist a b)
        b-c (g/dist b c)]
    (if upper-left
      (gp/polygon2 [a
                    b
                    (v/+polar b b-c angle)
                    (v/+polar a b-c angle)])
      (gp/polygon2 [a
                    (v/+polar a a-b (- angle tm/HALF_PI))
                    (v/+polar d a-b (- angle tm/HALF_PI))
                    d]))))

(defn gen-box [{:keys [affine]} {[width height] :size :as bounds} existing]
  (let [w (dr/random-int (* 0.04 width) (int (* 0.44 width)))
        h (dr/random-int (* 0.04 height) (int (* 0.44 height)))
        rect (rect/rect (dr/random-int 0 (- width w))
                        (dr/random-int 0 (- height h))
                        w h)
        poly (if (dr/chance (if affine 0.5 0.05))
               (let [angle (* (dr/rand-nth [1 -1]) (dr/random 2 4))]
                 (parallelogram rect (dr/chance 0.33) (/ Math/PI angle)))
               rect)
        box (if (dr/chance (if affine 0.5 0.2))
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

(defn heading-to [p q]
  (g/heading (tm/- q p)))

(defn triangle-arc [triangle]
  (let [[a b c] (take 3 (drop (dr/random-int 3) (cycle (g/vertices triangle))))
        mid-bc (tm/mix b c 0.5)
        p (tm/mix a mid-bc 0.5)
        r (* (dr/random 0.9 1.0) (g/dist p mid-bc))
        ap (g/dist a p)
        a-angle (triangle/law-of-sines-angle (/ Math/PI 6) r ap)
        proj (triangle/law-of-cosines-side r ap (- Math/PI (/ Math/PI 6) a-angle))
        isec-ab (v/+polar a proj (heading-to a b))
        isec-ac (v/+polar a proj (heading-to a c))]
    (arc/arc p r (heading-to p isec-ab) (heading-to p isec-ac))))

;; FIXME: need to extend to closest point on the clipping window, leaves gaps sometimes
(defn partitioned-arcs [windows]
  (fn [{:keys [p r] :as arc}]
    (let [overlapping (filter (fn [w] (collide/overlaps? (g/bounds arc) w))  windows)
          point-in-window? (fn [p] (some (fn [window] (g/contains-point? window p)) overlapping))]
      (->> (g/vertices arc 64)
           (drop 1)
           (partition-by point-in-window?)
           (filter (fn [pts] (and (point-in-window? (first pts))
                                 (not (tm/delta= (first pts) (last pts))))))
           (map (fn [pts] (arc/arc p r (heading-to p (first pts)) (heading-to p (last pts)))))))))

(defn dashed-arc [pattern]
  (fn [{:keys [r] :as arc}]
    (->> pattern
         cycle
         (map (fn [x] (/ 1.0 (* 8 x))))
         (reductions +)
         (take-while (fn [l] (<= l 1.0)))
         (partition 2 2)
         (mapcat (fn [[a b]] [[:M (g/point-at arc a)]
                             [:A [r r] 0.0 0 1 (g/point-at arc b)]]))
         csvg/path)))

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

        line-density (dr/gaussian (* width 0.08) 6.0)
        hatch-density (dr/gaussian (* width 0.03) 2.5)
        cross-density (dr/gaussian (* width 0.015) 1.5)

        boxes (generate bounds (partial gen-box {:affine (dr/chance 0.33)}) 20)
        lines (clip/hatch-rectangle bounds line-density theta0)
        circles (swap-triangles (generate bounds gen-circle 18) (dr/random-int 5))
        [windows shapes] (if (dr/chance 0.75)
                           [boxes circles]
                           [circles boxes])
        shapes (map (fn [s] (vary-meta s assoc :fill (dr/rand-nth palette)
                                      :cross (dr/chance 0.08)))
                    shapes)
        clipped-shapes (mapcat (fn [window] (clipped window shapes)) windows)
        inner-lines
        (mapcat (fn [line]
                  (let [subset (mapcat (fn [shape] (lines/clip-line line shape)) clipped-shapes)]
                    (if (dr/chance 0.15)
                      (mapcat (fn [segment] (dashed-line segment [2 3 5])) subset)
                      subset)))
                (clip/hatch-rectangle bounds hatch-density theta1))

        crossed
        (clip-lines-to-shapes
         (clip/hatch-rectangle bounds cross-density theta2)
         (filter (fn [s] (:cross (meta s))) clipped-shapes))
        arcs (->> shapes
                  (filter (fn [x] (instance? Triangle2 x)))
                  (mapcat (comp (partitioned-arcs windows) triangle-arc))
                  (map (dashed-arc [3 1 4])))]
    [(csvg/group {:fill background
                  :stroke-width 1.5}
       windows)
     (csvg/group {}
       (map (fn [s] (vary-meta s dissoc :cross)) clipped-shapes))
     (csvg/group {:stroke-width 0.5}
       (mapcat (fn [line] (separate line windows)) lines))
     (csvg/group {:stroke-width 0.5}
       inner-lines)
     (csvg/group {:stroke-width 0.125}
       crossed)
     (csvg/group {:stroke-width 0.9} arcs)]))

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
      [:<>
       [:div.canvas-frame [scene palette]]
       [:div.contained
        [:div.flexcols {:style {:justify-content :space-evenly :align-items :center}}
         [view-sketch/generate :window-glimpses]
         [palette/as-svg {} palette]]]])))

(sketch/definition window-glimpses
  {:created-at "2023-05-18"
   :tags #{}
   :type :svg}
  (ctrl/mount page))
