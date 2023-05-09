(ns shimmers.sketches.radial-breaks
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn minimum-spacing [gap offsets]
  (->> offsets
       (partition 2 1)
       (filter (fn [[a b]] (> (- b a) gap)))
       (map second)
       (into [0])))

;; FIXME remove gap at theta transition back to 0
;; FIXME how to gather together slices and not just the breaks outward from center
(defn shapes []
  (let [radial (minimum-spacing 0.05 (map (partial * eq/TAU) (dr/gaussian-range 0.02 0.05)))
        breaks (minimum-spacing 0.01 (dr/gaussian-range 0.02 0.05))
        radius (* 0.45 height)]
    (for [[t0 t1] (partition 2 1 radial)
          [r0 r1] (partition 2 1 (dr/random-sample 0.4 breaks))]
      (csvg/arc-segment t0 t1 (* radius r0) (* radius r1)
                        {}))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 1.0}
    (csvg/group {:transform (csvg/translate (rv 0.5 0.5))}
      (shapes))))

(sketch/definition radial-breaks
  {:created-at "2023-05-08"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/static-page scene :radial-breaks)))
