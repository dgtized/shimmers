(ns shimmers.sketches.control-panels
  (:require
   [shimmers.algorithm.square-packing :as square]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn vu-meter [center r pct]
  (let [p (tm/+ center (gv/vec2 0 (* 0.25 r)))
        t0 (* (/ 7 6) Math/PI)
        t1 (* (/ 11 6) Math/PI)
        inner (* 0.75 r)
        upper (* 0.9 r)
        lower (* 0.8 r)
        theta (tm/mix* t0 t1 pct)]
    (svg/group {}
               (csvg/path [[:M (tm/+ p (v/polar r t0))]
                           [:A [r r] 0 0 1 (tm/+ p (v/polar r t1))]
                           [:L (tm/+ p (v/polar inner t1))]
                           [:A [inner inner] 0 0 0 (tm/+ p (v/polar inner t0))]
                           [:Z]])
               (for [t (map #(+ % 0.05) (range t0 t1 0.1))]
                 (g/translate (gl/line2 (v/polar lower t) (v/polar upper t))
                              p))
               (gl/line2 (tm/+ p (v/polar (* 0.5 r) theta))
                         (tm/+ p (v/polar (* 0.95 r) theta))))))

(defn knob [p r theta]
  (svg/group {}
             (gc/circle p r)
             (gl/line2 p (tm/+ p (v/polar r theta)))))

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
                      (knob (g/centroid s) (* 0.08 (g/width c)) (dr/random 0 Math/PI)))
                    (vu-meter (g/centroid t) (* 0.45 (g/height t)) (dr/random))))
            (for [s (g/subdivide c {:rows 4 :cols 2})]
              (gc/circle (g/centroid s) (* 0.12 (g/width c)))))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 0.75}
            (apply list (shapes))))

(sketch/definition control-panels
  {:created-at "2022-02-07"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :control-panels)
              "sketch-host"))
