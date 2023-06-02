(ns shimmers.sketches.chance-connections
  (:require
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
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

(defn path-segments [points]
  (let [points' (dr/shuffle points)]
    (loop [path (vec (take 1 points'))
           points (rest points')]
      (if (seq points)
        (let [current (last path)
              next (apply min-key (fn [v] (g/dist-squared current v)) points)]
          (recur (conj path next)
                 (remove (fn [v] (tm/delta= current v)) points)))
        path))))

(defn shapes [bounds]
  (let [points (rp/poisson-disc-sampling (g/scale-size bounds 0.95) 128)]
    (csvg/group {}
      (let [path (path-segments points)]
        (csvg/path (into [[:M (first path)]]
                         (map (fn [v] [:L v]) (rest path)))))
      (csvg/group {:fill "black"}
        (for [p points]
          (gc/circle p 2.0))))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.5}
    (shapes (rect/rect 0 0 width height))))

(defn page []
  [:<>
   [:div.canvas-frame [scene]]
   [:div.contained
    [:div.flexcols {:style {:justify-content :space-evenly :align-items :center}}
     [view-sketch/generate :chance-connections]
     [:p.readable-width
      "Create a set of random points using poisson disc sampling. Pick a random
     point to start and then greedily take the next closest point in the set
     from the current position until the set is exhausted."]]]])

(sketch/definition chance-connections
  {:created-at "2023-06-01"
   :tags #{}
   :type :svg}
  (ctrl/mount page))
