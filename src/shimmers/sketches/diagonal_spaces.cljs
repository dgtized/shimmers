(ns shimmers.sketches.diagonal-spaces
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.stair :as ms]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn row [a b n slant]
  (for [t (mapv (dr/weighted {identity 9.0
                              (partial ms/staircase (dr/random-int 2 11)) 1.0})
                (dr/weighted {(tm/norm-range n) 5.0
                              (dr/gaussian-range (/ 1.0 n) (/ 0.15 n) true) 1.0}))]
    (if (and (<= 0.05 t 0.95) (dr/chance 0.05))
      (gl/line2 (rv (- t slant) a) (rv (+ t slant) b))
      (gl/line2 (rv t a) (rv t b)))))

(defn shapes []
  (let [rows (dr/weighted {5 1 7 1 9 2 11 1 13 1 15 1})]
    (for [[a b] (partition 2 1 (dr/weighted {(tm/norm-range rows) 1.0
                                             (dr/gaussian-range (/ 1.0 rows) (/ 0.15 rows) true) 1.0}))]
      (let [gap (* 0.05 (- b a))
            ga (+ a gap)
            gb (- b gap)
            slant (dr/weighted {0.025 1 -0.025 1})
            n (dr/weighted {100 1 125 0.5 150 1 175 0.5 200 0.5 250 0.5})]
        (svg/group {}
                   (gl/line2 (rv 0 ga) (rv 1 ga))
                   (gl/line2 (rv 0 gb) (rv 1 gb))
                   (svg/group {} (row ga gb n slant)))))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 1.0}
    (shapes)))

(sketch/definition diagonal-spaces
  {:created-at "2025-11-29"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
