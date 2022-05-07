(ns shimmers.sketches.geometry-examples
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.utils :as gu]
   [thi.ng.math.core :as tm]))

;; FIXME: inlined from geom.utils, to explore problems with centroid of concave
;; polygons
(defn fit-all-into-bounds
  "Takes an AABB or rect and seq of shapes, proportionally scales and
  repositions all items to fit into given bounds. Returns lazyseq of
  transformed entities. Does not support collections of mixed 2D/3D
  entities. Use rects as target bounds for 2D colls."
  [bounds coll]
  (let [b (gu/coll-bounds coll)
        s (reduce min (tm/div (get bounds :size) (get b :size)))
        b' (g/center (g/scale b s) (g/centroid bounds))]
    (for [shape coll]
      (-> shape
          (g/center (g/unmap-point b' (g/map-point b (g/centroid shape))))
          (g/scale-size s)))))

(defn make-example [{:keys [given results] :as example}]
  (let [bounds (rect/rect 0 0 300 150)
        left (rect/rect 0 0 150 150)
        right (rect/rect 150 0 150 150)

        given-fit (fit-all-into-bounds left given)
        results-fit
        (mapcat (fn [place result]
                  (fit-all-into-bounds place result))
                (g/subdivide right {:cols (count results)})
                results)]
    (merge example
           {:size (:size bounds)
            :shapes (concat given-fit results-fit)})))

(def examples
  (let [convex-poly (gp/polygon2 [0 0] [10 0] [10 10] [8 10]
                                 [8 4] [2 4] [2 10] [0 10])
        lines {"horizontal-coincident" (gl/line2 [0 4] [10 4])
               "horizontal-low" (gl/line2 [0 2] [10 2])
               "horizontal-high" (gl/line2 [0 6] [10 6])
               "vertical-left" (gl/line2 [2 0] [2 10])
               "vertical-right" (gl/line2 [8 0] [8 10])
               "vertical-middle" (gl/line2 [5 0] [5 10])
               "diagonal-left" (gl/line2 [0 0] [10 10])
               "diagonal-low-left" (gl/line2 [0 3] [10 7])
               "diagonal-right" (gl/line2 [0 10] [10 0])
               "diagonal-low-right" (gl/line2 [0 7] [8 0])}]
    (for [[desc line] (sort-by first lines)]
      (make-example
       {:title (str "cut-polygon convex " desc)
        :given [convex-poly line]
        :results [(lines/cut-polygon convex-poly line)]}))))

(defn edn-list [xs]
  (into [:div {}]
        (mapv (fn [v] (debug/pre-edn v {:width 120}))
              xs)))

(defn show-example [{:keys [title description size given results shapes]}]
  (let [[width height] (or size [400 100])]
    [:div
     (when title [:h3 title])
     (csvg/svg {:width width
                :height height
                :stroke "black"
                :fill "white"
                :stroke-width 0.5}
               shapes)
     [:details
      [:summary "Given/Results"]
      (edn-list given)
      [:h4 "Results"]
      (edn-list results)]
     (when description
       [:p description])]))

(defn page []
  [:div
   [:h2 "Cut Polygon with a Line"]
   [:p.explanation "Visual test cases for cutting a polygon with a line, and
   separating it into the component polygons."]
   (into [:div {}] (mapv show-example examples))])

(sketch/definition geometry-examples
  {:created-at "2022-05-05"
   :type :svg
   :tags #{}}
  (ctrl/mount page "interface"))
