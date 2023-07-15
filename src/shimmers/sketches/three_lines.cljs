(ns shimmers.sketches.three-lines
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn invert-x [{[p q] :points}]
  (gl/line2 (- width (:x p)) (:y p) (- width (:x q)) (:y q)))

(defn lines []
  (let [top (gl/line2 (rv (dr/random -0.2 0.4) -0.1)
                      (rv (dr/random 0.2 0.6) (dr/random 0.6 0.9)))
        bottom (gl/line2 (rv (dr/random 0.6 1.2) 1.1)
                         (rv (dr/random 0.4 0.8) (dr/random 0.1 0.6)))
        flip-x (dr/chance 0.5)]
    [(if flip-x (invert-x top) top)
     (if flip-x (invert-x bottom) bottom)
     (gl/line2 (rv (dr/random -0.1 0.3) (dr/random 0.3 0.7))
               (rv (dr/random 0.7 1.1) (dr/random 0.3 0.7)))]))

(defn shapes []
  (lines))

(defn scene []
  (csvg/svg-timed
    {:width width
     :height height
     :stroke "black"
     :fill "white"
     :stroke-width 32.0}
    (shapes)))

(sketch/definition three-lines
  {:created-at "2023-07-15"
   :tags #{}
   :type :svg}
  (ctrl/mount (view-sketch/static-page scene :three-lines)))
