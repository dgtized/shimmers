(ns shimmers.sketches.layered-intersections
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.math.geometry.intersection :as isec]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(def margin 8)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn invert [x] (- 1.0 x))

(defn random-offset []
  ((dr/rand-nth [invert identity]) (/ 1 (dr/rand-nth [2 3 4 5 6 7]))))

(defn cut-line [line offset padding]
  (let [margin (/ padding (tm/mag line))]
    [(gl/line2 (g/point-at line 0.0) (g/point-at line (- offset margin)))
     (gl/line2 (g/point-at line (+ offset margin)) (g/point-at line 1.0))]))

(defn perpindicular [line offset]
  (let [point (g/point-at line offset)]
    (-> line
        (g/translate (tm/- point))
        (g/rotate tm/HALF_PI)
        (g/translate point))))

(defn map-point [{[p _] :points :as line} point]
  (let [closest (g/closest-point line point)]
    (/ (g/dist p closest) (tm/mag line))))

(defn space-divide [bounds]
  (let [start (gv/vec2 0 (random-offset))
        end (gv/vec2 1.0 (random-offset))
        offset (random-offset)
        line (gl/line2 (g/unmap-point bounds start)
                       (g/unmap-point bounds end))
        r (* (g/height bounds) 0.01 (+ 2 (dr/random-int 6)))
        circle (gc/circle (g/point-at line offset) r)
        perp-line (first (lines/clip-line (g/scale-size (perpindicular line offset) 3.0) bounds))
        isec (isec/line-intersect line perp-line)
        p-isec (map-point perp-line isec)]
    {:circle [circle]
     :lines (concat (cut-line line offset r)
                    (cut-line perp-line p-isec r))}))

(defn overlap-lines [layer-lines lines]
  (reduce (fn [lines overlap]
            (mapcat (fn [line]
                      (if-let [isec (isec/line-intersect line overlap)]
                        (cut-line line (map-point line isec) margin)
                        [line]))
                    lines))
          lines
          layer-lines))

(defn cut-before [line isec-pt padding]
  (g/point-at line (- (map-point line isec-pt) (/ padding (tm/mag line)))))

(defn cut-after [line isec-pt padding]
  (g/point-at line (+ (map-point line isec-pt) (/ padding (tm/mag line)))))

(defn overlap-circles [circles lines]
  (reduce (fn [lines circle]
            (mapcat (fn [{[p q] :points :as line}]
                      (if-let [{:keys [type isec]} (isec/circle-ray circle p q)]
                        (do
                          (println type isec p q)
                          (case type
                            :tangent (cut-line line (map-point line (first isec)) margin)
                            :poke [(gl/line2 p (cut-before line (first isec) margin))]
                            :exit [(gl/line2 (cut-after line (first isec) margin) q)]
                            :impale [(gl/line2 p (cut-before line (first isec) margin))
                                     (gl/line2 (cut-after line (second isec) margin) q)]
                            :inside []
                            [line]))
                        [line]))
                    lines))
          lines
          circles))

(defn build-layers [bounds]
  (reduce (fn [layers _]
            (let [{:as attempt}
                  (->> #(space-divide bounds)
                       repeatedly
                       (drop-while
                        (fn [attempt]
                          (some (fn [{[circle] :circle}]
                                  (when (collide/overlaps? (g/scale-size circle 1.1)
                                                           (first (:circle attempt)))
                                    attempt))
                                layers)))
                       first)]
              (conj layers attempt)))
          []
          (range (dr/random-int 3 8))))

(defn add-gaps [layers]
  (reduce
   (fn [existing layer]
     (conj existing
           (-> layer
               (update :lines
                       (fn [lines]
                         (overlap-lines
                          (mapcat :lines existing)
                          lines)))
               (update :lines
                       (fn [lines]
                         (overlap-circles
                          (mapcat :circle existing)
                          lines))))))
   []
   (reverse layers)))

(defn shapes [bounds]
  (->> bounds
       build-layers
       add-gaps
       (mapcat (fn [layer] (apply concat (vals layer))))))

(defn scene []
  (let [bounds (rect/rect 0 0 width height)]
    (csvg/svg-timed {:width (g/width bounds)
                     :height (g/height bounds)
                     :stroke "black"
                     :fill "none"
                     :stroke-width 1.0}
      (shapes bounds))))

(sketch/definition layered-intersections
  {:created-at "2023-06-08"
   :tags #{}
   :type :svg}
  (ctrl/mount (view-sketch/static-page scene :layered-intersections)))
