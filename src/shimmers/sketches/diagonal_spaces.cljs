(ns shimmers.sketches.diagonal-spaces
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [shimmers.math.deterministic-random :as dr]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn row [a b slant]
  (for [t (tm/norm-range 100)]
    (if (and (<= 0.1 t 0.9) (dr/chance 0.05))
      (gl/line2 (rv (- t slant) a) (rv (+ t slant) b))
      (gl/line2 (rv t a) (rv t b)))))

(defn shapes []
  (let [gap 0.015]
    (for [[a b] (partition 2 1 (tm/norm-range 5))]
      (let [ga (+ a gap)
            gb (- b gap)]
        (svg/group {}
                   (gl/line2 (rv 0 ga) (rv 1 ga))
                   (gl/line2 (rv 0 gb) (rv 1 gb))
                   (svg/group {} (row ga gb (dr/weighted {0.025 1 -0.025 1}))))))))

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
