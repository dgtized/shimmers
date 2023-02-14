(ns shimmers.sketches.deco-screens
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]
   [shimmers.math.deterministic-random :as dr]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn horizontal-dist [[x0 _] [x1 _]]
  (- x1 x0))

(defn vertical-dist [[_ y0] [_ y1]]
  (- y1 y0))

(defn dir [start size p]
  (dr/weighted {(gv/vec2 size 0.0)
                (if (< (horizontal-dist p start) size) 1 0)
                (gv/vec2 (- size 0.0) 0.0)
                (if (> (horizontal-dist p start) (- size)) 1 0)
                (gv/vec2 0.0 size) 2}))

(defn deco-path [start end size]
  (let [steps (dec (int (/ (g/dist start end) size)))]
    (csvg/path (concat [[:M start]]
                       (->> start
                            (iterate
                             (fn [p] (tm/+ p (dir start size p))))
                            rest
                            (take steps)
                            (mapv (fn [p] [:L p])))
                       [[:L end]]))))

(defn shapes []
  [(deco-path (rv 0.5 0.0) (rv 0.5 1.0) (* 0.1 width))])

(defn scene []
  (csvg/timed
   (csvg/svg {:width width
              :height height
              :stroke "black"
              :fill "white"
              :stroke-width 0.5}
     (shapes))))

(sketch/definition deco-screens
  {:created-at "2023-02-13"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :deco-screens)
              "sketch-host"))
