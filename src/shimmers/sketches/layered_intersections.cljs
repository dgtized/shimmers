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
     :lines (cut-line line offset r)
     :perp (cut-line perp-line p-isec r)}))

(defn shapes [bounds]
  (mapcat (fn [layer] (apply concat (vals layer)))
          (reduce (fn [layers _]
                    (let [{:as attempt}
                          (->> #(space-divide bounds)
                               repeatedly
                               (drop-while
                                (fn [attempt]
                                  (some (fn [{[circle] :circle}]
                                          (when (collide/overlaps? circle (first (:circle attempt)))
                                            attempt))
                                        layers)))
                               first)]
                      (conj layers attempt)))
                  []
                  (range 5))))

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
