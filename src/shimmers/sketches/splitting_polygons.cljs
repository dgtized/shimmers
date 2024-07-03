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

(defn set-arc-lengths [polygons]
  (map (fn [poly]
         (vary-meta poly
                    update :arc-length
                    (fn [l] (or l (g/circumference poly)))))
       polygons))

(defn recursive-split [rule steps polygons]
  (as-> polygons _
    (set-arc-lengths _)
    (sort-by (fn [polygon] (get (meta polygon) :arc-length))
             (fn [a b] (compare b a))
             _)
    (iterate
     (fn [polys]
       (let [choice (first polys)]
         (sort-by (fn [polygon] (get (meta polygon) :arc-length))
                  (fn [a b] (compare b a))
                  (concat (set-arc-lengths (split-polygon choice (rule choice)))
                          (rest polys)))))
     _)
    (nth _ steps)
    (map (fn [s] (vary-meta s dissoc :arc-length)) _)))

(defn shapes [bounds]
  (let [evens (recursive-split even-rule 64 [(g/scale-size bounds 0.9)])]
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
                   :stroke-width 2.5}
    (shapes (csvg/screen width height))))

(defn page []
  (fn []
    [sketch/with-explanation
     [:div.canvas-frame [scene]]
     [:div.center
      [view-sketch/generate :splitting-polygons]]
     [:p]
     [:div.centered.readable-width
      "Experimenting with technique outlined by Piter Pasma in "
      [:a {:href "https://piterpasma.nl/articles/polysub"}
       "How to split polygons unevenly"]]]))

(sketch/definition splitting-polygons
  {:created-at "2024-01-15"
   :tags #{}
   :type :svg}
  (ctrl/mount page))
