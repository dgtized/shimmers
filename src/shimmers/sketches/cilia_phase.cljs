(ns shimmers.sketches.cilia-phase
  (:require
   [clojure.math :as math]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; Concept: generate a random phase-shifted function, and then add cilia along
;; it's path. Specifically interested in trying to make another phase shifted
;; function and slowly modulate that over each cilia from left to right.

(def width 900)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn spline-fx []
  (let [rx (dr/weighted {1.0 1.0 0.5 1.0 2.0 1.0})
        rx' (dr/weighted {1.0 1.0 0.5 1.0 2.0 1.0})
        amp (dr/weighted {0.1 1.0 0.2 1.0 0.3 1.0 0.4 1.0})
        cx (dr/random)
        cx' (dr/random)]
    (fn [x] (math/sin (* eq/TAU (+ (* rx x) cx (* amp (math/cos (* eq/TAU (+ (* rx' x) cx'))))))))))

(defn screen-space [fx x]
  (rv x (+ 0.5 (* 0.35 (fx x)))))

(defn base-spline [fx]
  (for [x (range 0 1 0.0025)]
    (screen-space fx x)))

;; How to avoid intersecting cilia?
(defn cilias [fx]
  (let [spx (spline-fx)]
    (for [x (range 0 1 0.005)]
      (let [pt (screen-space fx x)
            pt' (screen-space fx (+ x 0.0001))
            len (+ 12 (* 8 (spx x)))
            offset (tm/normalize (g/rotate (tm/- pt' pt) (* eq/TAU 0.25)) len)]
        (gl/line2 (tm/+ pt offset) (tm/- pt offset))))))

(defn shapes []
  (let [fx (spline-fx)
        spline-pts (base-spline fx)
        cilia (cilias fx)]
    (concat [(csvg/path (csvg/segmented-path spline-pts))]
            cilia)))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed
   {:id scene-id
    :width width
    :height height
    :stroke "black"
    :fill "white"
    :stroke-width 1.0}
   (shapes)))

(sketch/definition cilia-phase
  {:created-at "2024-10-24"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
