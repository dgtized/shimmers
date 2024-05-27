(ns shimmers.sketches.window-glimpses
  (:require
   [clojure.math :as math]
   [reagent-keybindings.keyboard :as kb]
   [shimmers.algorithm.hand-drawn :as hand-drawn]
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.palette :as palette]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.svg-export :as svg-export]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.core :as sm]
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
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defonce ui-state (ctrl/state {:monochrome false
                               :path-points false}))

(defn vary-width [shape width]
  (vary-meta shape assoc :stroke-width width))

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
                 (parallelogram rect (dr/chance 0.33) (/ math/PI angle)))
               rect)
        box (if (dr/chance (if affine 0.5 0.2))
              (geometry/rotate-around-centroid poly (dr/gaussian 0.0 0.08))
              poly)]
    (if (collide/bounded? bounds box)
      (distinct-shape 1.15 existing box)
      existing)))

(defn seed-boxes [bounds]
  (let [width (dr/rand-nth [0.1 0.16 0.2])
        height (dr/rand-nth [0.08 0.10 0.12])]
    (case (dr/rand-nth [:bookends :topends :descending])
      :bookends
      [(rect/rect (g/unmap-point bounds (gv/vec2 0.1 0.1))
                  (g/unmap-point bounds (gv/vec2 (+ 0.1 width) 0.9)))
       (rect/rect (g/unmap-point bounds (gv/vec2 (- 0.9 width) 0.1))
                  (g/unmap-point bounds (gv/vec2 0.9 0.9)))]
      :topends
      [(rect/rect (g/unmap-point bounds (gv/vec2 0.2 0.05))
                  (g/unmap-point bounds (gv/vec2 0.8 (+ 0.1 height))))
       (rect/rect (g/unmap-point bounds (gv/vec2 0.2 (- 0.95 height)))
                  (g/unmap-point bounds (gv/vec2 0.8 0.95)))]
      :descending
      [(rect/rect (g/unmap-point bounds (gv/vec2 0.1 0.1))
                  (g/unmap-point bounds (gv/vec2 (+ 0.1 width) 0.6)))
       (rect/rect (g/unmap-point bounds (gv/vec2 (- 0.5 (/ width 2.0)) 0.25))
                  (g/unmap-point bounds (gv/vec2 (+ 0.5 (/ width 2.0)) 0.75)))
       (rect/rect (g/unmap-point bounds (gv/vec2 (- 0.9 width) 0.4))
                  (g/unmap-point bounds (gv/vec2 0.9 0.9)))])))

(defn gen-circle [{[width height] :size} existing]
  (let [r (int (* (min width height) (dr/random 0.05 0.40)))
        circle (gc/circle (dr/random-int r (- width r))
                          (dr/random-int r (- height r))
                          r)]
    (distinct-shape 1.1 existing circle)))

(defn seed-circles [{[width height] :size :as bounds}]
  (case (dr/rand-nth [:triplets :diagonal :spiral])
    :triplets
    [(gc/circle (gv/vec2 (* width 0.15) (* height 0.5)) (* height 0.15))
     (gc/circle (gv/vec2 (* width 0.5) (* height 0.5)) (* height 0.25))
     (gc/circle (gv/vec2 (* width 0.85) (* height 0.5)) (* height 0.15))]
    :diagonal
    (let [line (dr/rand-nth [(gl/line2 (g/unmap-point bounds (gv/vec2 0 0.1))
                                       (g/unmap-point bounds (gv/vec2 1 0.9)))
                             (gl/line2 (g/unmap-point bounds (gv/vec2 0 0.9))
                                       (g/unmap-point bounds (gv/vec2 1 0.1)))])
          radius (* height (dr/random 0.1 0.2))]
      (for [p [0.2 0.5 0.8]]
        (gc/circle (g/point-at line p) radius)))
    :spiral
    (let [center (g/unmap-point bounds (gv/vec2 0.32 0.70))
          n 7]
      (for [t (tm/norm-range n)
            :let [theta (* eq/TAU (math/sqrt t))]]
        (vary-meta (gc/circle (v/+polar center (* 0.12 height (math/pow tm/PHI (/ (* 2 theta) math/PI))) theta)
                              (* height 0.08 (math/sqrt (/ theta eq/TAU))))
                   assoc :spiral true)))))

(defn generate [bounds f seed-fn n]
  (let [seed (if seed-fn (seed-fn bounds) [])
        expected (max n (inc (count seed)))]
    (->> seed
         (iterate (partial f bounds))
         (take-while (fn [s] (< (count s) expected)))
         last)))

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
      (if (instance? Circle2 s)
        (assoc (meta s) :arc s)
        (meta s)))))

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
                    (collide/intersecting-points rp rq))
               [rq])
       (partition 2 2)
       (map (fn [[p q]]
              (with-meta (gl/line2 p q) (meta line))))))))

(defn mass-vary [shapes field value]
  (map (fn [s] (vary-meta s assoc field value))
       shapes))

(defn heading-to [p q]
  (g/heading (tm/- q p)))

(defn triangle-arc [triangle]
  (let [{:keys [start-vertex face-dist center-pct]} (meta triangle)
        [a b c] (take 3 (drop start-vertex (cycle (g/vertices triangle))))
        mid-bc (tm/mix b c 0.5)
        p (tm/mix a mid-bc center-pct)
        r (* face-dist (g/dist p mid-bc))
        ap (g/dist a p)
        a-angle (triangle/law-of-sines-angle (/ math/PI 6) r ap)
        proj (triangle/law-of-cosines-side r ap (- math/PI (/ math/PI 6) a-angle))
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

(defn circle-alternates
  [circles n]
  (let [alternate-shape
        (fn [c]
          (let [faces (dr/weighted {3 1.0 6 1.0 8 1.0})]
            (case faces
              3 (-> (triangle/inscribed-equilateral c (dr/random-tau))
                    (vary-meta assoc
                               :start-vertex (dr/random-int 3)
                               :face-dist (dr/random 0.9 1.0)
                               :center-pct (dr/rand-nth [0.5 0.33 0.25])))
              6 (g/as-polygon c 6)
              8 (geometry/rotate-around-centroid (g/as-polygon c 8) (/ math/PI 8)))))
        [alternates circles'] (split-at n (dr/shuffle circles))]
    (into (mapv alternate-shape alternates) circles')))

(defn dashed-line [line pattern]
  (->> pattern
       cycle
       (map (fn [x] (/ 1.0 (* 8 x))))
       (reductions +)
       (take-while (fn [l] (<= l 1.0)))
       (partition 2 2)
       (mapcat (fn [[a b]] [[:M (g/point-at line a)] [:L (g/point-at line b)]]))
       csvg/path))

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
;; (separate-palette (:colors (palette/by-name :slate-shell-red-tan-yellow)))
;; (separate-palette (:colors (palette/by-name :orange-black-blue-shell-red)))
;; (separate-palette [])

(defn connect-lines [lines]
  (let [la (dr/rand-nth lines)
        lb (dr/rand-nth (remove #{la} lines))]
    (for [t (dr/density-range 0.05 0.15)]
      (gl/line2 (g/point-at la t) (g/point-at lb t)))))

(defn generate-layers [bounds palette]
  (let [[background & palette] (separate-palette palette)
        theta0 (dr/random-tau)
        theta1 (dr/gaussian (+ theta0 (/ eq/TAU 4)) (/ eq/TAU 8))
        theta2 (dr/gaussian (/ (+ theta0 theta1) 2) (/ eq/TAU 16))
        shadow-dir (v/polar (math/sqrt (dr/random (* width 0.02)))
                            theta2)

        line-density (dr/gaussian (* width 0.08) 6.0)
        hatch-density (dr/gaussian (* width 0.03) 2.5)
        cross-density (dr/gaussian (* width 0.015) 1.5)

        affine (dr/chance 0.33)

        boxes (-> bounds
                  (generate (partial gen-box {:affine affine})
                            (when (dr/chance 0.25) seed-boxes)
                            (dr/random-int 18 23)))
        circles (-> bounds
                    (generate gen-circle
                              (when (dr/chance 0.25) seed-circles)
                              (dr/random-int 16 21))
                    (circle-alternates (dr/random-int 7)))
        [windows shapes] (if (dr/chance 0.75)
                           [boxes circles]
                           [circles boxes])
        shapes
        (map (fn [s]
               (let [pct-area (/ (g/area s) (g/area bounds))]
                 (vary-meta s assoc
                            :palette-color (dr/random-int (count palette))
                            :shadow (dr/chance (* 0.5 (- 1.0 pct-area)))
                            :cross (dr/chance 0.08))))
             shapes)

        shadows
        (->> (filter (fn [s] (:shadow (meta s))) shapes)
             (map (fn [s] (-> (g/scale-size s 0.995)
                             (g/translate shadow-dir)
                             (with-meta (assoc (meta s) :shadow true))))))
        lines
        (map (fn [l] (vary-width l (tm/clamp (dr/pareto 0.5 2.0) 0.5 3.0)))
             (clip/hatch-rectangle bounds line-density theta0 [(dr/random) (dr/random)]))]
    {:bounds bounds
     :background background
     :palette palette
     :theta1 theta1
     :theta2 theta2
     :hatch-density hatch-density
     :cross-density cross-density
     :windows
     (map (fn [s]
            (-> s
                (vary-width (tm/clamp (dr/gaussian 1.5 1.0) 0.75 2.5))
                (vary-meta assoc :project
                           (when (dr/chance (* (if (instance? Circle2 s) 0.5 1.0) 0.2))
                             (tm/* shadow-dir 1.8)))))
          windows)
     :shapes shapes
     :shadows shadows
     :lines lines
     :connecting-lines (when (dr/chance (+ 0.33 (if affine 0.33 0)))
                         (connect-lines lines))
     :hatched-lines
     (map (fn [line] (vary-meta line assoc :dashed (dr/chance 0.15)))
          (clip/hatch-rectangle bounds hatch-density theta1 [(dr/random) (dr/random)]))
     :crossed-lines
     (clip/hatch-rectangle bounds cross-density theta2 [(dr/random) (dr/random)])}))

(defn clockwise-vertices
  "Force a clockwise ordering by sorting vertices around centroid.

  This only works if the polygon is convex. This is likely an artifact of
  `g/clip-with`."
  [convex-polygon]
  (let [centroid (g/centroid convex-polygon)]
    (->> convex-polygon
         g/vertices
         (sort-by (fn [v] (heading-to centroid v))))))

(defn debug-chunks [arc-groups]
  (for [g arc-groups]
    (csvg/group {}
      (gc/circle (first g) 1.0)
      (gc/circle (last g) 1.0))))

(defn rotate-to-max-dist-last
  "Rotate so the straight line return from Z to close the shape is probably in the
  last place."
  [group]
  (let [idx (->> group
                 (partition 2 1)
                 (map-indexed vector)
                 (apply max-key (fn [[_idx [a b]]] (g/dist a b))))]
    (cs/rotate (inc (first idx)) group)))

(comment
  (rotate-to-max-dist-last [(gv/vec2) (gv/vec2 10 0)
                            (gv/vec2 5 5) (gv/vec2 0 5)]))

(defn rotate-groups [arc-groups]
  (if (= (count arc-groups) 1)
    [(rotate-to-max-dist-last (first arc-groups))]
    arc-groups))

(defn arc-path [polygon {:keys [p r]} attribs show-path-points]
  (let [on-arc? (fn [v] (tm/delta= (g/dist p v) r 0.0001))
        vertices (clockwise-vertices polygon)
        arc-groups (partition-by on-arc? vertices)
        commands
        (mapcat
         (fn [chunk]
           (if (and (on-arc? (first chunk)) (> (count chunk) 1))
             (let [a0 (heading-to p (first chunk))
                   a1 (heading-to p (last chunk))
                   large-arc (if (< (sm/clockwise-distance a0 a1) math/PI) 0 1)]
               [[:L (first chunk)]
                [:A [r r] 0.0 large-arc 1 (last chunk)]])
             (map (fn [v] [:L v]) chunk)))
         (rotate-groups arc-groups))
        path (csvg/path (conj (assoc-in (vec commands) [0 0] :M) [:Z])
                        attribs)]
    (if show-path-points
      (csvg/group {:n (count vertices)}
        (into [path] (debug-chunks arc-groups)))
      path)))

(defn clean-meta [s]
  (vary-meta s dissoc
             :arc :cross :palette-color :shadow
             :project :spiral
             :start-vertex :face-dist :center-pct))

(defn render-shapes [palette show-path-points]
  (fn [s]
    (let [{:keys [arc palette-color]} (meta s)
          fill (when (seq palette)
                 (nth palette (mod palette-color (count palette))))
          s' (clean-meta s)
          attribs (meta s')]
      (cond (and arc (not (instance? Circle2 s')))
            (arc-path s' arc (assoc attribs :fill fill) show-path-points)
            fill
            (vary-meta s' assoc :fill fill)
            :else
            s'))))

(defn shapes
  [{:keys [windows shapes shadows
           lines connecting-lines crossed-lines hatched-lines
           background palette show-path-points]}]
  (let [clipped-shapes
        (->> windows
             (mapcat (fn [window] (clipped window shapes)))
             (filter (fn [s] (seq (g/vertices s)))))

        clipped-shadows
        (->> windows
             (mapcat (fn [window] (clipped window shadows)))
             (filter (fn [s] (seq (g/vertices s)))))

        inner-lines
        (mapcat (fn [line]
                  (let [subset (mapcat (fn [shape] (lines/clip-line line shape))
                                       clipped-shapes)]
                    (if (:dashed (meta line))
                      (map (fn [segment] (dashed-line segment [2 3 5])) subset)
                      subset)))
                hatched-lines)

        crossed
        (clip-lines-to-shapes
         crossed-lines
         (filter (fn [s] (:cross (meta s))) clipped-shapes))
        arcs
        (->> shapes
             (filter (fn [x] (instance? Triangle2 x)))
             (mapcat (comp (partitioned-arcs windows) triangle-arc))
             (map (dashed-arc [3 1 4])))

        render-path
        (dr/rand-nth [(fn [line] (hand-drawn/squiggle-path {:displace-radius 5} line))
                      identity])]
    [(csvg/group {:stroke-width 0.5 :stroke "#888888"}
       (map render-path connecting-lines))
     (csvg/group {}
       (mapcat (fn [line] (separate line windows)) lines))
     (csvg/group {:fill background}
       (->> windows
            (filter (fn [s] (:project (meta s))))
            (mapcat (fn [s] (let [project (:project (meta s))
                                 back (g/translate s project)]
                             (into [back] (mapv gl/line2 (g/vertices s) (g/vertices back))))))))
     (csvg/group {:fill background}
       (map clean-meta windows))
     (csvg/group {} (map (render-shapes palette show-path-points) clipped-shadows))
     (csvg/group {} (map (render-shapes palette show-path-points) clipped-shapes))
     (csvg/group {:stroke-width 0.5} inner-lines)
     (csvg/group {:stroke-width 0.125} crossed)
     (csvg/group {:stroke-width 0.9} arcs)]))

;; TODO: curate palettes for this sketch -- dark inner is often weird, and need
;; higher contrast between color pairs..
;; TODO: color a specific strip between two of the inner stripe lines across all
;; shapes it's clipped with.
;; TODO: clip out pie shapes from circles that do not clip any other shape?
(defn scene [{:keys [bounds] :as layers}]
  [:<>
   [kb/kb-action "alt-s" #(svg-export/download "scene" "window-glimpses")]
   (csvg/svg-timed {:id "scene"
                    :width (g/width bounds)
                    :height (g/height bounds)
                    :stroke "black"
                    :fill "none"
                    :stroke-width 1.0}
     (shapes layers))])

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
        :green-shades
        :grey-charcoal-tan-mint-olive
        :brown-tan-shell-blue-brown
        :yellow-brown
        :shell-olive-blue-charcoal-red
        :blue-grey-red-white-lavender
        :teals-shell-yellow-blue
        :charcoal-grey-yellow-blue
        :sand-sky-water-black-grey
        :grey-brown-yellow-grey-blue
        :tan-shell-forest-green-offwhite]
       palette/by-names
       (map :colors)
       (concat [["#ffeedd" "#ddeeff"]])
       dr/rand-nth))

(defn page []
  (let [palette (pick-palette)
        bounds (rect/rect 0 0 width height)
        layers (generate-layers bounds palette)]
    (fn []
      [:<>
       [:div.canvas-frame
        (let [{:keys [monochrome path-points]} @ui-state]
          [scene
           (cond-> layers
             monochrome
             (assoc :background "white" :palette [])
             path-points
             (assoc :show-path-points path-points))])]
       [:div.contained
        [:div.evencols
         [view-sketch/generate :window-glimpses]
         [:div
          [palette/as-svg {} palette]
          [ctrl/checkbox ui-state "Monochrome" [:monochrome]]
          [ctrl/checkbox ui-state "Path Points" [:path-points]]]]]])))

(sketch/definition window-glimpses
  {:created-at "2023-05-18"
   :tags #{}
   :type :svg}
  (ctrl/mount page))
