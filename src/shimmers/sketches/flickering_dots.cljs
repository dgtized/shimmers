(ns shimmers.sketches.flickering-dots
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 800)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn subdivide [p r]
  (if (or (> p 0.66) (dr/chance p))
    [r]
    (mapcat (fn [rs]
              (subdivide (+ p (dr/random 0.075 0.225)) rs))
            (g/subdivide r {:rows 2 :cols 2}))))

(defn shapes [bounds]
  (let [squares
        (mapcat (fn [r] (subdivide 0.33 r))
                (g/subdivide bounds {:rows 10 :cols 10}))]
    [(csvg/group {}
       (for [s squares]
         (g/scale-size s 0.9)))
     (csvg/group {:stroke "none"
                  :fill "white"}
       (for [s squares
             v (g/vertices s)
             :let [r (/ (g/width s) 8)]]
         (gc/circle v r)))]))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "black"
                   :stroke-width 0.5}
    (shapes (g/scale-size (csvg/screen width height) 0.95))))


(defn explanation [_]
  [:div {:style {:width "75ch"}}
   [:p "Genuary 2025 - Day 19 - Op Art"]
   [:p "Using the "
    [:a {:href "https://en.wikipedia.org/wiki/Grid_illusion"} "Grid Illusion"]
    " as a basis for some optical illusion art. The grid is then selectively
    subdivided for several iterations. The eye is tricked into finding black
    dots at different intersections."]])

(sketch/definition flickering-dots
  {:created-at "2025-01-19"
   :tags #{:genuary2025}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args explanation scene)))
