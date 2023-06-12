(ns shimmers.sketches.triangle-intersections
  (:require
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]
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

(defn make-triangles [bounds n]
  (let [points (rp/random-points bounds n)]
    (map (partial fit-triangle bounds)
         points)))

(defn shapes [bounds]
  (let [triangles (make-triangles (g/scale-size bounds 0.9) 5)]
    [(csvg/group {:fill-opacity 0.15 :fill "#CCCCCC"} triangles)
     (csvg/group {:fill-opacity 0.15 :fill "#FFFFFF"}
       (->> triangles
            (map g/as-polygon)
            cs/all-pairs
            (mapcat (fn [[a b]]
                      [(g/clip-with a b)
                       (g/clip-with b a)]))
            (remove (fn [poly] (empty? (g/vertices poly))))
            (map (fn [poly] (g/translate poly (dr/jitter 4.0))))))]))

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
