(ns shimmers.sketches.cilia-phase
  (:require
   [clojure.math :as math]
   [shimmers.common.sequence :as cs]
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

(defn screen-space [y amp fx x]
  (rv x (+ y (* amp (fx x)))))

(defn base-spline [screen-space fx]
  (for [x (range 0 1 0.0025)]
    (screen-space fx x)))

(defn cilia-line [pt angle len]
  (let [offset (g/rotate (gv/vec2 len 0) angle)]
    (gl/line2 (tm/+ pt offset) (tm/- pt offset))))

(defn cilia-line-plot [spline pt angle len]
  (let [offset (g/rotate (gv/vec2 len 0) angle)
        axis (g/rotate (gv/vec2 (* 0.075 len) 0) (+ angle (* eq/TAU 0.25)))
        a (tm/+ pt offset)
        b (tm/- pt offset)]
    (csvg/path
     (csvg/segmented-path
      (for [x (range 0 1 0.02)]
        (let [fy (spline x)]
          (tm/+ (tm/mix a b x)
                (tm/mix (tm/- axis) axis fy))))))))

;; How to avoid intersecting cilia?
(defn cilias [screen-space cilia-spline fx spx c-amp theta-x]
  (for [x (range -0.05 1.05 0.005)]
    (let [pt (screen-space fx x)
          pt' (screen-space fx (+ x 0.0001))
          rotation (* 0.125 math/PI (theta-x x))
          angle (+ (g/heading (tm/- pt' pt)) (* eq/TAU 0.25) rotation)
          len (* height (+ c-amp (* 0.75 c-amp (spx x))))]
      (cilia-spline pt angle len))))

(defn params []
  (dr/weighted
   {[[0.15 0.1 0.0075]
     [0.33 0.2 0.015]
     [0.5 0.25 0.0275]
     [0.66 0.2 0.015]
     [0.85 0.1 0.0075]]
    0.0
    [[0.25 0.2 0.015]
     [0.5 0.25 0.035]
     [0.75 0.2 0.015]]
    0.0
    (let [n (dr/weighted {(dr/random-int 3 8) 1.5
                          (dr/random-int 2 14) 1.0
                          (dr/random-int 2 20) 1.0})
          s (dr/random 1.0)
          amp (tm/clamp (dr/gaussian 0.35 0.05) 0.075 0.6)]
      (into []
            (for [y (cs/midsection (tm/norm-range n))]
              [y
               (* (/ 1.0 (inc n))
                  (+ 0.025 (eq/gaussian s 0.5 -0.4 y)))
               (* (/ 1.0 (inc n))
                  (+ 0.025 (eq/gaussian amp 0.5 -0.125 y)))])))
    1.0}))

(defn shapes []
  (let [fx (spline-fx)
        spx (spline-fx)
        cspx (spline-fx)
        theta-x (spline-fx)
        cilia-spline
        (dr/weighted {cilia-line 1.0
                      (partial cilia-line-plot cspx) 1.5})]
    (mapcat (fn [[y amp c-amp]]
              (let [screen (partial screen-space y amp)
                    spline-pts (base-spline screen fx)
                    cilia (cilias screen cilia-spline fx spx c-amp theta-x)]
                [(csvg/path (csvg/segmented-path spline-pts))
                 (csvg/group {:stroke-width 0.75}
                   cilia)]))
            (params))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed
    {:id scene-id
     :width width
     :height height
     :stroke "black"
     :fill "none"
     :stroke-width 1.0}
    (shapes)))

(sketch/definition cilia-phase
  {:created-at "2024-10-24"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
