(ns shimmers.sketches.geometry-examples
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; FIXME: inlined from geom.utils, to explore problems with centroid of concave
;; polygons
(defn fit-all-into-bounds
  "Takes an AABB or rect and seq of shapes, proportionally scales and
  repositions all items to fit into given bounds. Returns lazyseq of
  transformed entities. Does not support collections of mixed 2D/3D
  entities. Use rects as target bounds for 2D colls."
  [bounds coll]
  ;; check minimum bounds
  (let [b (update (gu/coll-bounds coll) :size tm/max (gv/vec2 1 1))
        s (reduce min (tm/div (get bounds :size) (get b :size)))
        b' (g/center (g/scale b s) (g/centroid bounds))
        concave (some poly-detect/concave? coll)]
    (for [shape coll
          ;; temporary hardcoded offset
          :let [center (if (and concave (> (count (:points shape)) 2))
                         (tm/+ (g/centroid shape) (gv/vec2 0 2))
                         (g/centroid shape))]]
      (-> shape
          (g/center (g/unmap-point b' (g/map-point b center)))
          (g/scale-size s)))))

(comment
  (let [horizontal (gl/line2 [0 5] [10 5])
        vertical (gl/line2 [5 0] [5 10])
        diagonal (gl/line2 [0 0] [10 10])
        bounds (rect/rect 0 0 8 8)]
    {:bounds-h (gu/coll-bounds [horizontal])
     :bounds-v (gu/coll-bounds [vertical])
     :bounds-d (gu/coll-bounds [diagonal])
     :fit-h (fit-all-into-bounds bounds [horizontal])
     :fit-v (fit-all-into-bounds bounds [vertical])
     :fit-d (fit-all-into-bounds bounds [diagonal])}))

(defn mark-centroids [shapes]
  (for [s shapes]
    (vary-meta (gc/circle (g/centroid s) 0.1) assoc
               :fill "maroon"
               :stroke "none")))

(defn make-example [{:keys [given results] :as example}]
  (let [viewbox "0 0 10 10"]
    (merge example
           {:shapes [[:svg {:viewBox viewbox :x "5%" :width "40%"}
                      (concat given (mark-centroids given))]
                     [:svg {:viewBox viewbox :x "55%" :width "40%"}
                      (concat results (mark-centroids results))]]})))

(defn edn-list [xs]
  (into [:div {}]
        (mapv (fn [v] (debug/pre-edn v {:width 120}))
              xs)))

(def concave-poly (gp/polygon2 [0 0] [10 0] [10 10] [8 10]
                               [8 4] [2 4] [2 10] [0 10]))

(def hexagon (g/as-polygon (gc/circle [5 5] 5) 6))

(def lines {"horizontal-coincident" (gl/line2 [0 4] [10 4])
            "horizontal-low" (gl/line2 [0 2] [10 2])
            "horizontal-high" (gl/line2 [0 6] [10 6])
            "vertical-left" (gl/line2 [2 0] [2 10])
            "vertical-right" (gl/line2 [8 0] [8 10])
            "vertical-middle" (gl/line2 [5 0] [5 10])
            "diagonal-left" (gl/line2 [0 0] [10 10])
            "diagonal-low-left" (gl/line2 [0 3] [10 7])
            "diagonal-right" (gl/line2 [0 10] [10 0])
            "diagonal-low-right" (gl/line2 [0 7] [8 0])})

(defn cut-polygon-examples []
  (concat (for [[desc line] (sort-by first lines)]
            (make-example
             {:title (str "cut-polygon concave " desc)
              :given [concave-poly line]
              :results (lines/cut-polygon concave-poly line)}))
          (for [[desc line] (sort-by first lines)]
            (make-example
             {:title (str "cut-polygon hexagon " desc)
              :given [hexagon line]
              :results (lines/cut-polygon hexagon line)}))))

(defn clip-line-examples []
  (for [[desc line] (sort-by first lines)]
    (make-example
     {:title (str "clip-line concave " desc)
      :given [concave-poly line]
      :results (lines/clip-line line concave-poly)})))

(defn show-example [{:keys [title description given results shapes]}]
  [:div
   (when title [:h3 title])
   (csvg/svg {:width 400
              :height 200
              :stroke "black"
              :fill "white"
              :stroke-width 0.05}
             (apply list shapes))
   [:details
    [:summary "Given/Results"]
    (edn-list given)
    [:h4 "Results"]
    (edn-list results)]
   (when description
     [:p description])])

(defn page []
  [:div
   [:div
    [:h2 "Cut Polygon with a Line"]
    [:p.explanation "Visual test cases for cutting a polygon with a line, and
   separating it into the component polygons."]
    (into [:div {}] (mapv show-example (cut-polygon-examples)))]
   [:div
    [:h2 "Clip lines to segments inside of a polygon"]
    (into [:div {}] (mapv show-example (clip-line-examples)))]])

(sketch/definition geometry-examples
  {:created-at "2022-05-05"
   :type :svg
   :tags #{}}
  (ctrl/mount page "interface"))
