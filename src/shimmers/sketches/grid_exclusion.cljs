(ns shimmers.sketches.grid-exclusion
  (:require
   [shimmers.algorithm.square-packing :as square]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn shapes [squares remaining]
  (if (or (> (count squares) 200) (empty? remaining))
    squares
    (let [{[w h] :size :as rect}
          (->> remaining
               (filter (fn [{:keys [size]}] (every? #(> % 20) size)))
               (dr/weighted-by g/area))
          area (* w h)
          min-size (geometry/min-axis rect)
          [sq & panes]
          (square/split-panes
           rect
           (int (cond (> area 1400)
                      (* min-size (dr/rand-nth [0.2 0.25 0.33 0.5]))
                      (> area 800)
                      (* min-size (dr/rand-nth [0.5 0.66 0.75 0.8]))
                      :else
                      min-size))
           [0 0] (dr/weighted {(square/row-major rect) 2
                               :all 1}))]
      (recur (conj squares sq)
             (into (remove #{rect} remaining)
                   (filter square/has-area? panes))))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 1.0}
    (shapes [] [(g/scale-size (csvg/screen width height) 0.95)])))

(sketch/definition grid-exclusion
  {:created-at "2022-11-01"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/static-page scene :grid-exclusion)))
