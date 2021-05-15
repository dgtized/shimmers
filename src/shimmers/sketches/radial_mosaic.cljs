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

(defn segment [origin t0 t1 r0 r1]
  (let [rot (tm/degrees (- t1 t0))]
    (svg/path [[:M (tm/+ origin (polar r0 t0))]
               [:L (tm/+ origin (polar r1 t0))]
               [:L (tm/+ origin (polar r1 t1))]
               #_[:A [r0 r0 rot "1" "1" x1 y1]]
               [:L (tm/+ origin (polar r0 t1))]
               [:L (tm/+ origin (polar r0 t0))]
               #_[:A [r0 r0 rot "1" "1" x1 y1]]
               [:Z]]
              {:fill "none" :stroke "black"})))

(defn scene [origin]
  (csvg/svg {:width width :height height}
            (gc/circle origin 10)
            (mapcat (fn [[dt r0 r1]]
                      (for [[t0 t1] (radial-range dt)]
                        (segment origin t0 t1 r0 r1)))
                    [[0.5 11 16]
                     [0.8 19 27]
                     [0.3 28 36]
                     [0.4 37 52]
                     [0.6 54 64]
                     [0.15 66 78]
                     [0.25 80 100]])))

(defn page []
  [:div (scene (r 0.5 0.5))])

(defn ^:export run-sketch []
  ;; 20210409
  (ctrl/mount page "svg-host"))
