(ns shimmers.sketches.polar-accession
  (:require
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn sweep-arc [c r t0 t1]
  (let [[t0 t1]
        (if (< t0 0.0)
          [(+ t0 eq/TAU) [(+ t1 eq/TAU)]]
          [t0 t1])
        large-arc (if (<= (abs (- t1 t0)) (* 0.5 eq/TAU)) 0 1)
        sweep (if (> t0 t1) 0 1)]
    (svg/path [[:M (v/+polar c r t0)]
               [:A [r r] 0.0 large-arc sweep
                (v/+polar c r t1)]])))

(defn shapes []
  (let [center (rv 0.5 0.5)
        radius (* 0.475 height)]
    (for [r (range 0.15 1.0 0.15)
          [a b] (cs/pair-cycle (dr/gaussian-range 0.125 0.025))
          :let [b (if (> a b) (+ 1.0 b) b)
                gap (* 0.1 (- b a))]]
      (sweep-arc center (* r radius)
                 (* eq/TAU (+ a gap))
                 (* eq/TAU (- b gap))))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 5}
    (shapes)))

(sketch/definition polar-accession
  {:created-at "2026-01-10"
   :tags #{:genuary2026}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
