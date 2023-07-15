(ns shimmers.sketches.three-lines
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn invert-x [{[p q] :points}]
  (gl/line2 (- width (:x p)) (:y p) (- width (:x q)) (:y q)))

(defn lines []
  (let [top (gl/line2 (rv (dr/random -0.2 0.4) (dr/random -0.2 0.1))
                      (rv (dr/random 0.2 0.6) (dr/random 0.6 0.9)))
        bottom (gl/line2 (rv (dr/random 0.6 1.2) (dr/random 0.9 1.2))
                         (rv (dr/random 0.4 0.8) (dr/random 0.1 0.6)))
        flip-x (dr/chance 0.5)]
    (dr/shuffle
     [(if flip-x (invert-x top) top)
      (if flip-x (invert-x bottom) bottom)
      (gl/line2 (rv (dr/random -0.1 0.35) (dr/random 0.25 0.75))
                (rv (dr/random 0.65 1.1) (dr/random 0.25 0.75)))])))

(defn grey [t]
  (str "hsla(0,0%," (int (* 100 t)) "%,66%)"))

(defn draw-line [{[p q] :points}]
  (for [_ (range 45)]
    (-> (gl/line2 (tm/mix p q (dr/gaussian 0.0 0.07))
                  (tm/mix p q (dr/gaussian 1.0 0.07)))
        (g/translate (dr/jitter (dr/gaussian 8.0 4.0)))
        (vary-meta assoc
                   :stroke-width (tm/clamp (dr/gaussian 7.0 4.0) 1.0 32.0)
                   :stroke (grey (tm/clamp01 (dr/gaussian 0.02 0.2)))))))

(defn shapes []
  [(csvg/group {:stroke-opacity 0.03} (mapcat draw-line (lines)))
   (csvg/group {:stroke-opacity 0.12} (mapcat draw-line (lines)))
   (csvg/group {:stroke-opacity 1.00} (mapcat draw-line (lines)))])

(defn scene []
  (csvg/svg-timed
    {:width width
     :height height
     :stroke "black"
     :fill "white"}
    (shapes)))

(sketch/definition three-lines
  {:created-at "2023-07-15"
   :tags #{}
   :type :svg}
  (ctrl/mount (view-sketch/static-page scene :three-lines)))
