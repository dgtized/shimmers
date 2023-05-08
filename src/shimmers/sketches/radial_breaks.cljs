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

(defn segment [t0 t1 r0 r1 attribs]
  (csvg/arc-segment t0 t1 r0 r1
                    (merge
                     {:fill "white"
                      :stroke-width 0.6
                      :stroke "black"}
                     attribs)))

(defn shapes []
  (let [breaks (drop 1 (dr/gaussian-range 0.01 0.05))
        radius (* 0.45 height)]
    (for [[t0 t1] (partition 2 1 (rest (dr/gaussian-range 0.01 0.05)))
          [r0 r1] (partition 2 1 (dr/random-sample 0.4 breaks))]
      (csvg/arc-segment (* eq/TAU t0) (* eq/TAU t1) (* radius r0) (* radius r1)
                        {}))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.5}
    (csvg/group {:transform (csvg/translate (rv 0.5 0.5))}
      (shapes))))

(sketch/definition radial-breaks
  {:created-at "2023-05-08"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/static-page scene :radial-breaks)))
