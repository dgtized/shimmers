(ns shimmers.sketches.triangle-intersections
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [thi.ng.geom.rect :as rect]
   [shimmers.algorithm.random-points :as rp]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn angles [shape]
  (let [vertices (g/vertices shape)]
    (for [[a b c] (->> (concat (take-last 1 vertices)
                               vertices
                               (take 1 vertices))
                       (partition 3 1))]
      (let [p (tm/- a b)
            q (tm/- c b)]
        (Math/acos (/ (tm/dot p q)
                      (* (abs (tm/mag p))
                         (abs (tm/mag q)))))))))

;; Exclude obtuse triangles
(defn fit-triangle [{:as bounds} point]
  (let [triangle (apply gt/triangle2 (conj (rp/random-points bounds 2) point))]
    (if-not (some (fn [x] (> x tm/HALF_PI)) (angles triangle))
      triangle
      (recur bounds point))))

(defn shapes [bounds]
  (let [bounds' (g/scale-size bounds 0.9)
        points (rp/random-points bounds' 5)]
    (map (partial fit-triangle bounds')
         points)))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 1.0}
    (shapes (rect/rect 0 0 width height))))

(sketch/definition triangle-intersections
  {:created-at "2023-06-12"
   :tags #{}
   :type :svg}
  (ctrl/mount (view-sketch/static-page scene :triangle-intersections)))
