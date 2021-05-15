(ns shimmers.sketches.radial-mosaic
  (:require [shimmers.common.svg :as csvg]
            [shimmers.common.ui.controls :as ctrl]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.svg.core :as svg]))

(def width 900)
(def height 600)
(defn r [x y]
  (gv/vec2 (* x width) (* y height)))

(defn polar [r theta]
  (geom/as-cartesian (gv/vec2 r theta)))

(defn radial-range [dt]
  (let [r (range 0 tm/TWO_PI dt)]
    (conj (vec (map vec (partition 2 1 r))) [(last r) (first r)])))

(defn scene [origin]
  (csvg/svg {:width width :height height}
            (gc/circle origin 10)
            (for [[t0 t1] (radial-range 0.5)
                  :let [rot (tm/degrees (- t1 t0))
                        [x1 y1] (tm/+ origin (polar 11 t1))]]
              (svg/path [[:M (tm/+ origin (polar 11 t0))]
                         [:L (tm/+ origin (polar 16 t0))]
                         [:L (tm/+ origin (polar 16 t1))]
                         #_[:A [11 11 rot "1" "1" x1 y1]]
                         [:L (tm/+ origin (polar 11 t1))]
                         [:L (tm/+ origin (polar 11 t0))]
                         #_[:A [11 11 rot "1" "1" x1 y1]]
                         [:Z]]
                        {:fill "none" :stroke "black"}))))

(defn page []
  [:div (scene (r 0.5 0.5))])

(defn ^:export run-sketch []
  ;; 20210409
  (ctrl/mount page "svg-host"))
