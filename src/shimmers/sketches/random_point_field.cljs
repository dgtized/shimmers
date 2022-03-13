(ns shimmers.sketches.random-point-field
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn random-points [bounds n]
  (repeatedly n #(g/unmap-point bounds (gv/vec2 (dr/random) (dr/random)))))

;; Generates *close* to n points
(defn random-cells [bounds n]
  (let [cells (g/subdivide bounds {:num (Math/ceil (Math/sqrt n))})]
    (for [cell cells]
      (g/unmap-point cell (gv/vec2 (dr/random) (dr/random))))))

(defonce ui-state (ctrl/state {:mode :random-points
                               :n-points 512}))
(def modes {:random-points random-points
            :random-cells random-cells} )

(defn scene []
  (let [bounds (g/scale-size (rect/rect 0 0 width height) 0.99)
        {:keys [mode n-points]} @ui-state
        point-cloud (get modes mode)
        points (point-cloud bounds n-points)]
    (println (count points))
    (csvg/svg {:width width
               :height height
               :stroke "black"
               :fill "white"
               :stroke-width 0.5}
              (svg/group {:fill "black"}
                         (map (fn [p] (gc/circle p 1.5)) points)))))

(defn ui-controls []
  [:div
   [:p "Various methods of generating a random set of points in a boundary"]
   [:h4 "Controls"]
   (ctrl/change-mode ui-state (keys modes))
   (ctrl/numeric ui-state "Generated Points" [:n-points] [2 1024 1])])

(sketch/definition random-point-field
  {:created-at "2022-05-12"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :random-point-field ui-controls)
              "sketch-host"))
