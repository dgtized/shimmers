(ns shimmers.sketches.geometry-examples
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.geometry.polygon :as poly]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

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
     shapes)
   [:details
    [:summary "Given/Results"]
    (edn-list given)
    [:h4 "Results"]
    (edn-list results)]
   (when description
     [:p description])])

(defn inset-arc-example []
  (let [width 400 height 200
        triangle
        (triangle/inscribed-equilateral (gc/circle (gv/vec2 (* width 0.5) (* height 0.5))
                                                   (* height 0.45))
                                        tm/PI)
        [c a b] (g/vertices triangle)
        mid-bc (tm/mix b c 0.5)
        p (tm/mix a mid-bc 0.5)
        r (g/dist p mid-bc)
        ap (g/dist a p)
        c-angle (triangle/law-of-sines-angle (/ tm/PI 6) r ap)
        proj (triangle/law-of-cosines-side r ap (- tm/PI (/ tm/PI 6) c-angle))
        isec-ab (v/+polar a proj (g/heading (tm/- b a)))
        isec-ac (v/+polar a proj (g/heading (tm/- c a)))]
    (csvg/svg {:width width :height height :stroke "black" :fill "none"}
      (concat [triangle
               (csvg/path [[:M isec-ab]
                           [:A [r r] 0.0 0.0 1.0 isec-ac]])
               (csvg/path [[:M p] [:L mid-bc]] {:id "p-mid-bc"})
               [:text [:textPath {:href "#p-mid-bc" :startOffset "50%"} [:tspan {:dy "-0.25em"} "r"]]]
               (csvg/path [[:M p] [:L isec-ab]] {:id "p-isec-ab"})
               [:text [:textPath {:href "#p-isec-ab" :startOffset "50%"} [:tspan {:dy "-0.25em"} "r"]]]
               (csvg/path [[:M p] [:L isec-ac]] {:id "p-isec-ac"})
               [:text [:textPath {:href "#p-isec-ac" :startOffset "50%"} [:tspan {:dy "-0.25em"} "r"]]]]
              (for [c [a mid-bc p isec-ab isec-ac]]
                (gc/circle c 2.0))))))

(defn regular-polygons-example []
  (let [width 600 height 200]
    (csvg/svg {:width width :height height :stroke "black" :fill "none"}
      (for [n (range 3 11)]
        (let [r (poly/circumradius-side-length n 20)]
          (-> (poly/regular-n-gon n r)
              (g/center (tm/+ (gv/vec2 (* 2 r) (* height 0.5))
                              (tm/* (gv/vec2 width 0) (/ (- n 3) 9))))))))))

(defn page []
  [:div.contained.explanation
   [:div
    [:h2 "Cut Polygon with a Line"]
    [:p.readable-width "Visual test cases for cutting a polygon with a line, and
   separating it into the component polygons. Red points mark the centroid of
   each polygon or line."]
    (into [:div {}] (mapv show-example (cut-polygon-examples)))]
   [:div
    [:h2 "Clip lines to segments inside of a polygon"]
    (into [:div {}] (mapv show-example (clip-line-examples)))]
   [:div
    [:h2 "Inset arc in triangle"]
    [:p "Given an equilateral triangle pick an inner point along an axis, and
    then draw an arc with radius the distance between the point and the midpoint
    of the opposing face. "]
    (inset-arc-example)]
   [:div
    [:h2 "Regular Polygons"]
    (regular-polygons-example)
    ]])

(sketch/definition geometry-examples
  {:created-at "2022-05-05"
   :type :svg
   :tags #{:demo}}
  (ctrl/mount page))
