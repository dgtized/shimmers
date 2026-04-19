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
   [thi.ng.geom.line :as gl]
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
                             (dr/random (- tm/QUARTER_PI) tm/QUARTER_PI))))))))

;; FIXME: is there a way to trim out the vertical segments when placing shapes?
(defn zig-zag [bounds rows]
  (let [row-height (/ 1.0 (dec rows))]
    (gl/linestrip2
     (mapcat (fn [i]
               (let [p (g/unmap-point bounds (gv/vec2 0 (* i row-height)))
                     q (g/unmap-point bounds (gv/vec2 1.0 (* i row-height)))]
                 (if (even? i)
                   [p q]
                   [q p])))
             (range rows)))))

(defn path-shape []
  (dr/weighted
   {(gc/circle (rv 0.5 0.5) (* height 0.415)) 1.0
    (zig-zag (g/center (g/scale-size (csvg/screen width height) 0.75)
                       (rv 0.5 0.5))
             (dr/random-int 4 7)) 1.0}))

(defn morph [from to t]
  (for [v (butlast (tm/norm-range 32))]
    (tm/mix (g/point-at from v) (g/point-at to v) t)))

(defn shapes []
  (let [shape-xs (shape-seq)
        path-shape (path-shape)]
    (for [t (butlast (tm/norm-range 24))]
      (let [base (int (* t (count shape-xs)))]
        (g/translate (gp/polygon2 (morph (nth shape-xs base)
                                         (nth shape-xs (mod (inc base) (count shape-xs)))
                                         (mod (* t (count shape-xs)) 1.0)))
                     (g/point-at path-shape t))))))

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
