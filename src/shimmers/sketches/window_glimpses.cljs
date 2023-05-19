(ns shimmers.sketches.window-glimpses
  (:require
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.sketch :as sketch :include-macros true]
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

(defn gen-box [{[width height] :size} existing]
  (let [w (dr/random-int (* 0.04 width) (int (* 0.44 width)))
        h (dr/random-int (* 0.04 height) (int (* 0.44 height)))
        poly (g/as-polygon
              (rect/rect (dr/random-int 0 (- width w))
                         (dr/random-int 0 (- height h))
                         w h))
        box (if (dr/chance 0.2)
              (geometry/rotate-around-centroid poly (dr/gaussian 0.0 0.08))
              poly)]
    (if (some (fn [s] (collide/overlaps? (g/scale-size box 1.12) s)) existing)
      existing
      (conj existing box))))

(defn gen-circle [{[width height] :size} existing]
  (let [r (dr/random-int (int (* 0.04 (min width height)))
                         (int (* 0.44 (min width height))))
        circle (gc/circle (dr/random-int r (- width r))
                          (dr/random-int r (- height r))
                          r)]
    (if (some (fn [s] (collide/overlaps? (g/scale-size circle 1.05) s)) existing)
      existing
      (conj existing circle))))

(defn generate [bounds f n]
  (->> []
       (iterate (partial f bounds))
       (take-while (fn [s] (< (count s) n)))
       last))

(defn clipped [shape shapes]
  (for [s shapes
        :when (collide/overlaps? shape s)]
    (g/clip-with shape s)))

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

(defn shapes [bounds]
  (let [theta0 (dr/random-tau)
        theta1 (dr/gaussian (+ theta0 (/ eq/TAU 4)) (/ eq/TAU 8))
        boxes (generate bounds gen-box 20)
        lines (clip/hatch-rectangle bounds (* width 0.08) theta0)
        circles (map (fn [s] (g/as-polygon s 64))
                     (generate bounds gen-circle 16))
        [as bs] (dr/shuffle [boxes circles])
        clipped-bs (mapcat (fn [a] (clipped a bs)) as)
        inner-lines
        (mapcat (fn [line]
                  (mapcat (fn [shape] (lines/clip-line line shape)) clipped-bs))
                (clip/hatch-rectangle bounds (* width 0.03) theta1))]
    (concat
     (mass-vary (mapcat (fn [line] (separate line as)) lines)
                :stroke-width 0.5)
     (mass-vary as :stroke-width 1.5)
     clipped-bs
     (mass-vary inner-lines :stroke-width 0.5))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 1.0}
    (shapes (rect/rect 0 0 width height))))

(sketch/definition window-glimpses
  {:created-at "2023-05-18"
   :tags #{}
   :type :svg}
  (ctrl/mount (view-sketch/static-page scene :window-glimpses)))
