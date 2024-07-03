(ns shimmers.sketches.position-out-of-phase
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn grid [seed rect size scale]
  (for [r (g/subdivide rect size)]
    (let [box (g/scale-size r 0.66)
          motion (* 0.5 (g/width box))
          c (g/centroid r)
          p1 c
          p2 (tm/+ c (gv/vec2 (g/width rect) (g/height rect)))]
      (g/translate box
                   (v/polar (- (* motion (dr/noise-at-point seed scale p1)) (* 0.5 motion))
                            (* eq/TAU (dr/noise-at-point seed scale p2)))))))

(defn shapes [bounds]
  (let [s-bounds (g/scale-size bounds 0.95)
        seed (dr/noise-seed)
        size1 {:rows 48 :cols 64}
        size2 {:rows 24 :cols 32}
        size3 {:rows 12 :cols 16}
        size4 {:rows 6 :cols 8}
        size5 {:rows 3 :cols 4}]
    (concat (grid seed s-bounds size1 0.005)
            (grid seed s-bounds size1 0.00525)
            (grid seed s-bounds size1 0.0055)
            (grid seed s-bounds size2 0.003)
            (grid seed s-bounds size2 0.0031)
            (grid seed s-bounds size2 0.0032)
            (grid seed s-bounds size3 0.002)
            (grid seed s-bounds size3 0.0021)
            (grid seed s-bounds size3 0.0022)
            (grid seed s-bounds size4 0.001)
            (grid seed s-bounds size4 0.0011)
            (grid seed s-bounds size4 0.0012)
            (grid seed s-bounds size5 0.00050)
            (grid seed s-bounds size5 0.00055)
            (grid seed s-bounds size5 0.00060))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.5}
    (shapes (csvg/screen width height))))

(sketch/definition position-out-of-phase
  {:created-at "2024-06-25"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
