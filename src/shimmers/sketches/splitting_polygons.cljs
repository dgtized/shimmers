(ns shimmers.sketches.splitting-polygons
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; A3 ish aspect ratio
(def width 640)
(def height 900)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn split-polygon [polygon cut]
  (let [line (apply min-key tm/mag-squared
                    (repeatedly 5 (fn []
                                    (let [a (dr/random)]
                                      (gl/line2 (g/point-at polygon a)
                                                (g/point-at polygon (mod (+ a cut) 1.0)))))))]
    (lines/cut-polygon polygon line)))

(defn even-rule [_]
  (dr/gaussian 0.5 0.05))

(defn tunnel-rule [_]
  (dr/gaussian 0.2 0.02))

(defn recursive-split [rule steps polygons]
  (-> (fn [polygons]
        (let [xs (sort-by (fn [polygon] (get (meta polygon) :arc-length 0)) polygons)]
          (into (butlast xs)
                (for [child (split-polygon (last xs) (rule (last xs)))]
                  (vary-meta child assoc :arc-length (g/circumference child))))))
      (iterate polygons)
      (nth steps)))

(defn shapes [bounds]
  (let [evens (recursive-split even-rule 50 [(g/scale-size bounds 0.9)])]
    (mapcat
     (fn [polygon]
       (let [py (/ (:y (g/centroid polygon)) height)
             [rule steps]
             (cond (< py 0.125)
                   [tunnel-rule (dr/random-int 128 256)]
                   (< py 0.25)
                   [tunnel-rule (dr/random-int 64 128)]
                   (> py 0.75)
                   [even-rule (dr/random-int 256 1024)]
                   :else
                   [even-rule (if (dr/chance 0.1)
                                (dr/random-int 512 1024)
                                (dr/random-int 32 64))])]
         [polygon
          (csvg/group
            {:stroke-width 0.4}
            (recursive-split rule steps [polygon]))]))
     evens)))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 2.0}
    (shapes (rect/rect 0 0 width height))))

(defn page []
  (fn []
    [sketch/with-explanation
     [:div.canvas-frame [scene]]
     [:div.center
      [view-sketch/generate :splitting-polygons]]
     [:p]
     [:div.centered.readable-width
      "Experimenting with technique outlined by Piterpasma in "
      [:a {:href "https://piterpasma.nl/articles/polysub"}
       "How to split polygons unevenly"]]]))

(sketch/definition splitting-polygons
  {:created-at "2024-01-15"
   :tags #{}
   :type :svg}
  (ctrl/mount page))
