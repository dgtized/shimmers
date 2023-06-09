(ns shimmers.sketches.layered-intersections
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [shimmers.math.geometry.intersection :as isec]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn invert [x] (- 1.0 x))

(defn random-offset []
  ((dr/rand-nth [invert identity]) (/ 1 (dr/rand-nth [2 3 4 5]))))

(defn cut-line [line offset margin]
  [(gl/line2 (g/point-at line 0.0) (g/point-at line (- offset margin)))
   (gl/line2 (g/point-at line (+ offset margin)) (g/point-at line 1.0))])

(defn perpindicular [line]
  (let [center (g/centroid line)]
    (-> line
        (g/translate (tm/- center))
        (g/rotate tm/HALF_PI)
        (g/translate center))))

(defn space-divide [bounds]
  (let [start (gv/vec2 0 (random-offset))
        end (gv/vec2 1.0 (random-offset))
        offset (random-offset)
        line (gl/line2 (g/unmap-point bounds start)
                       (g/unmap-point bounds end))
        r (dr/random 0.02 0.10)
        circle (gc/circle (g/point-at line offset) (* (tm/mag line) r))
        perp-line (perpindicular line)
        isec (isec/line-intersect line perp-line)
        p-isec (g/map-point bounds isec)]
    (into (into [circle] (cut-line line offset r))
          (cut-line perp-line p-isec r))))

(defn shapes [bounds]
  (let [spaces (repeatedly 2 #(space-divide bounds))]
    (println (apply concat spaces))
    (apply concat spaces)))

(defn scene []
  (let [bounds (rect/rect 0 0 width height)]
    (csvg/svg-timed {:width (g/width bounds)
                     :height (g/height bounds)
                     :stroke "black"
                     :fill "white"
                     :stroke-width 1.0}
      (shapes bounds))))

(sketch/definition layered-intersections
  {:created-at "2023-06-08"
   :tags #{}
   :type :svg}
  (ctrl/mount (view-sketch/static-page scene :layered-intersections)))
