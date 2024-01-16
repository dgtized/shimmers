(ns shimmers.sketches.splitting-polygons
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [shimmers.algorithm.lines :as lines]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.utils :as gu]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(gu/arc-length-index [(gv/vec2 0 0) (gv/vec2 1 5) (gv/vec2 3 1)])

(defn split-polygon [polygon]
  (let [{:keys [a b]} (meta polygon)
        [ca cb] (sort [a b])
        dt (dr/random 0.1)]
    (for [poly (lines/cut-polygon polygon (gl/line2 (g/point-at polygon ca)
                                                    (g/point-at polygon cb)))]
      (vary-meta poly assoc
                 :a (mod (+ ca dt) 1.0)
                 :b (mod (+ cb dt) 1.0)))))

(defn recursive-split [polygons]
  (iterate (fn [polys]
             (dr/mapcat-random-sample
              (fn [polygon]
                (if (> (gu/arc-length (g/vertices polygon)) 20)
                  1.0 0.0))
              split-polygon
              polys))
           polygons))

(defn shapes []
  (let [t (dr/random)]
    (nth (recursive-split
          [(vary-meta (g/scale-size (rect/rect 0 0 width height) 0.9)
                      assoc
                      :a t
                      :b (mod (+ t (dr/gaussian 0.5 0.1)) 1.0))]) 20)))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.5}
    (shapes)))

(defn page []
  (fn []
    [sketch/with-explanation
     [:div.canvas-frame [scene]]
     [view-sketch/generate :splitting-polygons]
     [:div.readable-width
      "Experimenting with technique outlined by Piterasma in "
      [:a {:href "https://piterpasma.nl/articles/polysub"}
       "How to split polygons unevenly"]]]))

(sketch/definition splitting-polygons
  {:created-at "2024-01-15"
   :tags #{}
   :type :svg}
  (ctrl/mount page))
