(ns shimmers.sketches.follow-thy-neighbor
  (:require
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.collisions :as collide]
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
   (for [t (tm/norm-range 10)]
     (tm/mix (g/point-at left t) (g/point-at right t) displace))))

(defn average-dist [a b samples]
  (/ (reduce + (map (fn [t] (g/dist (g/point-at a t) (g/point-at b t)))
                    (tm/norm-range samples)))
     (inc samples)))

(defn subdivide [lines]
  (let [idx (->> lines
                 (partition 2 1)
                 (map-indexed
                  (fn [idx [a b]]
                    (let [dist (average-dist a b 3)]
                      [idx (if (< dist 10) (* dist 0.2) dist)])))
                 dr/weighted)
        [before after] (split-at (inc idx) lines)
        a (last before)
        b (first after)
        t (dr/random 0.25 0.75)
        line (make-line a b t)]
    (if (or (collide/overlaps? a line)
            (collide/overlaps? b line))
      lines
      (concat before [line] after))))

(defn shapes []
  (let [init
        (concat [(gl/line2 (rv 0.0 0.0) (rv 0.0 1.0))]
                (for [t (cs/midsection (tm/norm-range 3))]
                  (-> [(rv t 0.0)
                       (rv (+ t (dr/gaussian 0.0 0.04)) (dr/gaussian 0.33 0.03))
                       (rv (+ t (dr/gaussian 0.0 0.04)) (dr/gaussian 0.66 0.03))
                       (rv t 1.0)]
                      bezier/auto-spline2
                      (g/vertices 10)
                      gl/linestrip2
                      (vary-meta assoc :stroke-width 3.0)))
                [(gl/line2 (rv 1.0 0.0) (rv 1.0 1.0))])]
    (last (take 20 (iterate subdivide init)))))

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
