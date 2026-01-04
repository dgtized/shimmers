(ns shimmers.sketches.fibonnaci-days
  (:require
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn arc-segment [p q]
  #_(csvg/path [:M (v/+polar p r t0)])
  (gl/line2 p q))

(defn shapes []
  (let [lines (cs/midsection (tm/norm-range (inc 13)))]
    (concat (for [t lines]
              (gl/line2 (rv 0.2 t) (rv 0.8 t)))
            (map-indexed
             (fn [i [a b]]
               (let [x (if (odd? i) 0.2 0.8)]
                 (arc-segment (rv x a) (rv x b))))
             (partition 2 1 lines)))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 5}
    (shapes)))

(defn explanation []
  [:div
   [:p "Genuary 2026 - Day3 - Fibonnaci forever"]])

(sketch/definition fibonnaci-days
  {:created-at "2026-01-03"
   :tags #{:genuary2026}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args explanation scene)))
