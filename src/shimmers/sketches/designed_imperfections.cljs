(ns shimmers.sketches.designed-imperfections
  (:require
   [clojure.math :as math]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 900)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes []
  (let [seed (dr/noise-seed)
        center (rv 0.5 0.5)]
    (->> (for [box (g/subdivide (csvg/screen width height)
                                {:cols (/ width 6) :rows (/ height 6)})]
           (let [pos (g/centroid box)
                 n (dr/noise-at-point seed 0.008 pos)
                 damp (/ 1.0 (Math/pow 1.006 (g/dist pos center)))]
             (gc/circle (tm/+ pos
                              (tm/+ (v/polar (* 10.0 damp) (* eq/TAU n))
                                    (v/polar (* 20.0 (- 1.0 damp)) (g/heading (tm/- pos center)))))
                        (+ 2.0 (* 0.8 damp (math/sin (* eq/TAU (- 1.0 n))))))))
         (dr/map-random-sample (fn [{:keys [r]}]  (/ r 200.0))
                               (fn [{:keys [p r]}] (g/center (rect/rect (* 1.9 r)) p))))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 1.0}
    (shapes)))

(sketch/definition designed-imperfections
  {:created-at "2024-07-28"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
