(ns shimmers.sketches.follow-thy-neighbor
  (:require
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.bezier :as bezier]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn make-line [left right displace]
  (gl/linestrip2
   (for [t (tm/norm-range 6)]
     (tm/mix (g/point-at left t) (g/point-at right t) displace))))

(defn subdivide [lines]
  (let [idx (->> lines
                 (partition 2 1)
                 (map-indexed
                  (fn [idx [a b]]
                    [idx (g/dist (g/point-at a 0.5) (g/point-at b 0.5))]))
                 dr/weighted)
        [before after] (split-at (inc idx) lines)
        a (last before)
        b (first after)
        t (dr/random 0.25 0.75)
        line (make-line a b t)]
    (concat before [line] after)))

(defn shapes []
  (let [init
        (concat [(gl/line2 (rv 0.0 0.0) (rv 0.0 1.0))]
                (for [t (cs/midsection (tm/norm-range 3))]
                  (bezier/auto-spline2 [(rv t 0.0)
                                        (rv (+ t (dr/gaussian 0.0 0.05)) (dr/random 0.2 0.45))
                                        (rv (+ t (dr/gaussian 0.0 0.05)) (dr/random 0.55 0.8))
                                        (rv t 1.0)]))
                [(gl/line2 (rv 1.0 0.0) (rv 1.0 1.0))])]
    (last (take 32 (iterate subdivide init)))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 1.0}
    (shapes)))

(defn page []
  (fn []
    [sketch/with-explanation
     [:div.canvas-frame [scene]]
     [view-sketch/generate :follow-thy-neighbor]
     [:div.readable-width]]))

(sketch/definition follow-thy-neighbor
  {:created-at "2023-11-30"
   :tags #{}
   :type :svg}
  (ctrl/mount page))
