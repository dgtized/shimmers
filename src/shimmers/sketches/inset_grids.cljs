(ns shimmers.sketches.inset-grids
  (:require
   [clojure.math :as math]
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(def min-area (* 0.0015 width height))

(defn splits [{[w h] :size}]
  (if (dr/chance 0.33)
    (let [ratio (/ w h)
          n (dr/random-int 1 7)]
      {:rows (math/ceil (/ n ratio))
       :cols (math/ceil (* n ratio))})
    (if (> w h)
      {:rows (dr/random-int 1 5)
       :cols (dr/random-int 2 11)}
      {:rows (dr/random-int 2 11)
       :cols (dr/random-int 1 5)})))

(defn circles [rect]
  (map (fn [box]
         (let [r (* 0.33 (geometry/min-axis box))]
           (gc/circle (g/unmap-point box (gv/vec2 0.5 0.5)) r)))
       (g/subdivide rect (splits rect))))

(defn division [limit bounds]
  (let [rect (g/scale-size bounds 0.975)]
    (if (< (g/area rect) limit)
      (if (dr/chance 0.05)
        (into [rect]
              (clip/hatch-rectangle
               rect
               (* (dr/random 0.1 0.2) (geometry/min-axis rect))
               (+ (* 0.25 eq/TAU) (g/heading (tm/- (g/centroid rect) (rv 0.5 0.5))))
               [(dr/random) (dr/random)]))
        [rect])
      (mapcat (fn [r]
                (let [p-area (/ (g/area r) (* width height))
                      r' (if (dr/chance (* 0.3 (- 1.0 p-area)))
                           (let [d (dr/rand-nth [0.025 0.0125])]
                             (g/translate
                              r
                              (dr/rand-nth [(v/polar (* d (g/width r)) 0)
                                            (v/polar (* d (g/width r)) (* 0.5 eq/TAU))
                                            (v/polar (* d (g/height r)) (* 0.25 eq/TAU))
                                            (v/polar (* d (g/height r)) (* 0.75 eq/TAU))])))
                           r)]
                  (into [r'] (if (and (< 0.0001 p-area 0.04) (dr/chance 0.075))
                               (circles r')
                               (division limit r')))))
              (g/subdivide rect (splits rect))))))

(defn shapes [bounds]
  (division min-area bounds))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed
    {:id scene-id
     :width width
     :height height
     :stroke "black"
     :fill "none"
     :stroke-width 0.66}
    (shapes (csvg/screen width height))))

(sketch/definition inset-grids
  {:created-at "2024-07-16"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
