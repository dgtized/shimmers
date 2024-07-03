(ns shimmers.sketches.random-point-field
  (:require
   [loom.alg :as la]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.graph :as graph]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defonce ui-state
  (ctrl/state {:mode :random-points
               :n-points 512
               :mst false}))

(defn circle-between-closest [points]
  (for [[p q] (la/prim-mst-edges (graph/points->graph points))]
    (let [d (g/dist p q)]
      (gc/circle (tm/mix p q 0.5) (* 0.5 d)))))

(comment (circle-between-closest (rp/random-cells (rect/rect 0 0 10 10) 10)))

(defn scene [points mst]
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.5}
    [(csvg/group {:fill "black"}
       (map (fn [p] (gc/circle p 1.5)) points))
     (when mst
       (csvg/group {:fill "none"}
         (circle-between-closest points)))]))

(defn page []
  (let [bounds (g/scale-size (csvg/screen width height) 0.99)
        {:keys [mode n-points mst]} @ui-state
        point-cloud (get rp/modes mode)
        primes (rp/halton-prime-pair 20 60)
        points (if (= mode :halton-sequence)
                 (point-cloud bounds primes (* n-points 4))
                 (point-cloud bounds (or n-points 1)))]
    [:div
     [:div.canvas-frame [scene points mst]]
     [:div.explanation.contained
      [:div.flexcols
       [:div {:style {:width "40%"}}
        [view-sketch/generate :random-point-field]
        [:p "Various approaches of generating a random set of points in a boundary."]
        [:p (str "Generated " (count points) " points")]]
       [:div
        [:h4 "Controls"]
        (ctrl/change-mode ui-state (keys rp/modes))
        (ctrl/numeric ui-state "Generated Points" [:n-points] [2 1024 1])
        (when (= mode :halton-sequence)
          [:div "Halton sequence from co-primes " (debug/pre-edn primes)])
        (ctrl/checkbox ui-state "Show MST Circles" [:mst])]]]]))

(sketch/definition random-point-field
  {:created-at "2022-03-12"
   :type :svg
   :tags #{}}
  (ctrl/mount page))
