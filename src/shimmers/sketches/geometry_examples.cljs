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
        lines {"horizontal-coincident" (gl/line2 [0 4] [10 4])
               "horizontal-low" (gl/line2 [0 2] [10 2])
               "horizontal-high" (gl/line2 [0 6] [10 6])
               "vertical-left" (gl/line2 [2 0] [2 10])
               "vertical-right" (gl/line2 [8 0] [8 10])
               "vertical-middle" (gl/line2 [5 0] [5 10])
               "diagonal-left" (gl/line2 [0 0] [10 10])
               "diagonal-right" (gl/line2 [10 0] [0 10])}]
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
