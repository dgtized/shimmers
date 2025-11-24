(ns shimmers.sketches.blustery-day
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes [seed bounds]
  (for [cell (g/subdivide bounds {:cols (/ width 5)
                                  :rows (/ height 5)})
        :let [p (g/centroid cell)
              dir-noise (dr/noise-at-point-01 seed 0.0025 p)
              amp-noise (dr/noise-at-point-01 seed 0.0025 (tm/+ p (:size bounds)))
              size-noise (dr/noise-at-point-01 seed 0.0025 (tm/+ p (tm/* (:size bounds) 2)))
              jitter-noise (dr/noise-at-point-01 seed 0.0025 (tm/+ p (tm/* (:size bounds) 3)))
              shape-noise (dr/noise-at-point-01 seed 0.0025 (tm/+ p (tm/* (:size bounds) 4)))
              angle (* 1.5 eq/TAU dir-noise)]
        :when (< (dr/gaussian 0.0 (tm/smoothstep* 0.5 1.0 jitter-noise)) 1.0)]
    (let [c (gc/circle (-> p
                           (v/+polar (* 32.0 amp-noise) angle)
                           (v/+polar (* 0.75 (dr/gaussian 0.0 (tm/smoothstep* 0.5 1.0 jitter-noise)))
                                     (dr/random-tau)))
                       (+ 0.5 (* 2.0 size-noise)))]
      (cond (< shape-noise 0.2)
            (-> (* 1.1 (:r c))
                rect/rect
                g/center
                (g/rotate angle)
                (g/translate (:p c)))
            (> shape-noise 0.8)
            (triangle/inscribed-equilateral (:p c) (* 1.1 (:r c)) angle)
            :else c))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.75}
    (shapes (dr/noise-seed)
            (g/scale-size (csvg/screen width height) 0.925))))

(sketch/definition blustery-day
  {:created-at "2025-01-18"
   :tags #{:genuary2025}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
