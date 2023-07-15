(ns shimmers.sketches.three-lines
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
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

(defn expand-line [line]
  (let [length (tm/mag line)
        scale (cond (< length (* 0.5 width))
                    1.25
                    (< length (* 0.66 width))
                    1.1
                    :else
                    1.0)]
    (g/scale-size line scale)))

(defn lines []
  (let [top (gl/line2 (rv (dr/random -0.2 0.4) (dr/random -0.2 0.1))
                      (rv (dr/random 0.2 0.6) (dr/random 0.6 0.9)))
        bottom (gl/line2 (rv (dr/random 0.6 1.2) (dr/random 0.9 1.2))
                         (rv (dr/random 0.4 0.8) (dr/random 0.1 0.6)))
        flip-x (dr/chance 0.5)]
    (->> [(if flip-x (invert-x top) top)
          (if flip-x (invert-x bottom) bottom)
          (gl/line2 (rv (dr/random -0.1 0.3) (dr/random 0.25 0.75))
                    (rv (dr/random 0.7 1.1) (dr/random 0.25 0.75)))]
         dr/shuffle
         (mapv expand-line))))

(defn grey [t]
  (str "hsla(0,0%," (int (* 100 t)) "%,90%)"))

(defn rotate-around-point [{[p q] :points :as line} t angle]
  (geometry/rotate-around line (tm/mix p q t) angle))

(defn draw-line [{[p q] :points}]
  (for [_ (range 60)]
    (-> (gl/line2 (tm/mix p q (dr/gaussian 0.02 0.05))
                  (tm/mix p q (dr/gaussian 0.98 0.05)))
        (rotate-around-point (dr/gaussian 0.5 0.2) (dr/gaussian 0.0 0.02))
        (g/translate (dr/jitter (dr/gaussian 10.0 6.0)))
        (vary-meta assoc
                   :stroke-width (tm/clamp (dr/gaussian 4.0 6.0) 0.6 12.0)
                   :stroke (grey (tm/clamp01 (dr/gaussian 0.02 0.125)))))))

(defn shapes []
  [(csvg/group {:stroke-opacity 0.04
                :transform (csvg/rotate (dr/gaussian 0.0 1.5) (rv 0.5 0.5))}
     (mapcat draw-line (lines)))
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
