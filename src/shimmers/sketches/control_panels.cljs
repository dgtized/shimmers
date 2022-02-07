(ns shimmers.sketches.control-panels
  (:require
   [shimmers.algorithm.square-packing :as square]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [thi.ng.geom.circle :as gc]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes []
  (let [{p :p :as bounds} (g/scale-size (rect/rect 0 0 width height) 0.975)
        inner (rect/rect (g/unmap-point bounds (gv/vec2 0.3 0.0))
                         (g/unmap-point bounds (gv/vec2 0.7 1.0)))
        panes (square/surrounding-panes bounds (g/translate inner (tm/- p)) :column)
        [a c b] (mapv (fn [s] (with-meta (g/scale-size s 0.95) {:rx 10}))
                      (conj (->> panes
                                 (filter square/has-area?)
                                 (mapv (fn [s] (g/translate s p))))
                            inner))]
    (concat [a b c]
            (for [s (g/subdivide a {:rows 5 :cols 1})]
              (gc/circle (g/centroid s) (* 0.15 (g/width a))))
            (let [[t b] (g/subdivide b {:rows 2 :cols 1})]
              (conj (for [s (g/subdivide b {:rows 3 :cols 4})]
                      (gc/circle (g/centroid s) (* 0.08 (g/width c))))
                    (gc/circle (g/centroid t) (* 0.45 (g/height t)))))
            (for [s (g/subdivide c {:rows 4 :cols 2})]
              (gc/circle (g/centroid s) (* 0.12 (g/width c)))))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.75}
            (apply list (shapes))))

(sketch/definition control-panels
  {:created-at "2022-02-07"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :control-panels)
              "sketch-host"))
