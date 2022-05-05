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
   [thi.ng.geom.utils :as gu]))

(defn fit-example [given results]
  (let [bounds (rect/rect 0 0 300 150)
        left (rect/rect 0 0 150 150)
        right (rect/rect 150 0 150 150)
        shapes (concat (gu/fit-all-into-bounds left given)
                       (mapcat (fn [place result]
                                 (gu/fit-all-into-bounds place result))
                               (g/subdivide right {:cols (count results)})
                               results))]
    {:size (:size bounds)
     :shapes shapes}))

(defn make-example
  [{:keys [given results] :as example}]
  (merge example (fit-example given results)))

(def examples
  (let [convex-poly (gp/polygon2 [0 0] [10 0] [10 10] [8 10] [8 4] [2 4] [2 10] [0 10])
        horizontal (gl/line2 [0 4] [10 4])
        vertical (gl/line2 [2 0] [2 10])
        diagonal (gl/line2 [0 0] [10 10])]
    [(make-example
      {:title "cut-polygon convex horizontal"
       :description ""
       :given [convex-poly horizontal]
       :results [(lines/cut-polygon convex-poly horizontal)]})
     (make-example
      {:title "cut-polygon convex vertical"
       :description ""
       :given [convex-poly vertical]
       :results [(lines/cut-polygon convex-poly vertical)]})
     (make-example
      {:title "cut-polygon convex diagonal"
       :description ""
       :given [convex-poly diagonal]
       :results [(lines/cut-polygon convex-poly diagonal)]})]))

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
     (when description
       [:p description])
     [:div {}
      [:h4 "Given"]
      (edn-list given)
      [:h4 "Results"]
      (edn-list results)]]))

(defn page []
  (into [:div {}] (mapv show-example examples)))

(sketch/definition geometry-examples
  {:created-at "2022-05-05"
   :type :svg
   :tags #{}}
  (ctrl/mount page "interface"))
