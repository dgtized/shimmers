(ns shimmers.sketches.chance-connections
  (:require
   [shimmers.algorithm.chaikin :as chaikin]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
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
  (ctrl/state
   {:show-points false
    :chaikin false
    :depth 1}))

;; TODO: remove single intersection loops somehow? lookahead from an edge, if it
;; crosses only one other edge and no other intersecting edges in between,
;; splice connection and reverse the loop to connect without a loop?

;; Find all edge intersections, but include their index in the original edges?
;; Not sure how to deal with the edges that cross most of the sketch.
;; Basically need to unsnarl/planarize the connections

;; Also worth trying to find an euler tour?

;; Maybe cross-hatch the connections between each side of the resulting maze?
;; Or draw parallel lines to some edges, or mix and match between smooth and blocky?

(defn path-segments [points]
  (loop [path (vec (take 1 points))
         points (rest points)]
    (if (seq points)
      (let [current (last path)
            next (apply min-key (fn [v] (g/dist-squared current v))
                        points)]
        (recur (conj path next)
               (remove (fn [v] (tm/delta= next v)) points)))
      path)))

(defn point-path [{:keys [show-points]} points]
  (csvg/group {}
    (csvg/path (into [[:M (first points)]]
                     (map (fn [v] [:L v]) (rest points))))
    (when show-points
      (csvg/group {:fill "black"}
        (for [p points]
          (gc/circle p 2.0))))))

(defn scene [bounds points settings]
  (csvg/svg-timed {:width (g/width bounds)
                   :height (g/height bounds)
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.5}
    (let [{:keys [chaikin depth]} settings]
      (if chaikin
        (point-path settings (chaikin/chaikin 0.2 false depth points))
        (point-path settings points)))))

(defn page []
  (let [bounds (rect/rect 0 0 width height)
        points (path-segments (rp/poisson-disc-sampling (g/scale-size bounds 0.95) 256))]
    (fn []
      [:<>
       [:div.canvas-frame [scene bounds points @ui-state]]
       [:div.contained
        [:div.flexcols {:style {:justify-content :space-evenly :align-items :center}}
         [view-sketch/generate :chance-connections]
         [:div
          [:p.readable-width
           "Create a set of random points using poisson disc sampling. Pick the
     first point to start and then greedily take the next closest point in the
     set from the current position until the set is exhausted."]
          [ctrl/container {:style {:width "5em"}}
           [ctrl/checkbox ui-state "Show Points" [:show-points]]
           [:div.flexcols
            [ctrl/checkbox ui-state "Chaiken Smooth" [:chaikin]]
            (when (:chaikin @ui-state)
              [ctrl/numeric ui-state "Depth" [:depth] [1 6 1]])]]]]]])))

(sketch/definition chance-connections
  {:created-at "2023-06-01"
   :tags #{}
   :type :svg}
  (ctrl/mount page))
