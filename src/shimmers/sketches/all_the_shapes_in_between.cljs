(ns shimmers.sketches.all-the-shapes-in-between
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.polygon :as poly]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shape-seq []
  (let [radius (* 0.05 height)
        circle (gc/circle (gv/vec2) radius)]
    (take (dr/random-int 3 7)
          (dr/shuffle
           (into [circle]
                 (for [size (range 3 9)]
                   (g/rotate (poly/regular-n-gon size radius)
                             (dr/random-tau))))))))

(defn path [t]
  (g/point-at (gc/circle (rv 0.5 0.5) (* height 0.4)) t))

(defn morph [from to t]
  (for [v (butlast (tm/norm-range 32))]
    (tm/mix (g/point-at from v) (g/point-at to v) t)))

(defn shapes []
  (let [shape-xs (shape-seq)]
    (for [t (butlast (tm/norm-range 24))]
      (let [base (int (* t (count shape-xs)))]
        (g/translate (gp/polygon2 (morph (nth shape-xs base)
                                         (nth shape-xs (mod (inc base) (count shape-xs)))
                                         (mod (* t (count shape-xs)) 1.0)))
                     (path t))))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 1.0}
    (shapes)))

(defn explanation []
  [:div.evencols
   [:div.readable-width
    [:p "Create a set of N (3-6) shapes. The shapes can be a polygon of up to 8
   sides, or a circle. Once the shapres are selected, then morph between each of
   the shapes along a set path."]]])

(sketch/definition all-the-shapes-in-between
  {:created-at "2023-01-02"
   :type :svg
   :tags #{:genuary2023}}
  (ctrl/mount (-> sketch-args
                  (usvg/with-explanation explanation)
                  (usvg/page scene))))
