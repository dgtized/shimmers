(ns shimmers.sketches.path-following
  (:require
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.bezier :as bezier]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; https://en.wikipedia.org/wiki/Visvalingam%E2%80%93Whyatt_algorithm

(def width 800)
(def height 600)
(defn r [x y]
  (gv/vec2 (* width x) (* height y)))

(def original
  (-> [(r 0.1 0.5) (r 0.3 0.3) (r 0.6 0.6) (r 0.9 0.5)]
      bezier/auto-spline2
      (g/sample-uniform 10.0 true)
      gl/linestrip2
      (g/translate (r 0.0 -0.1))))

(defn dampened [{path :points} factor]
  (gl/linestrip2 (if (> (count path) 2)
                   (->> (for [[a b c d e] (partition 5 1 path)]
                          (tm/mix c (tm/mix (tm/mix a b) (tm/mix d e)) factor))
                        (cs/sandwich 2 path))
                   path)))

;; https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
(defn perpendicular-distance [[x1 y1] [x2 y2] [x0 y0]]
  (let [x21 (- x2 x1)
        y21 (- y2 y1)]
    (/ (Math/abs (- (* x21 (- y1 y0))
                    (* (- x1 x0) y21)))
       (Math/sqrt (+ (* x21 x21) (* y21 y21))))))

(defn max-perpendicular-distance [points]
  (let [a (first points)
        b (last points)]
    (loop [i 1 index 0 max-dist 0]
      (if (>= i (dec (count points)))
        [index max-dist]
        (let [pt (nth points i)
              d (perpendicular-distance a b pt)]
          (if (> d max-dist)
            (recur (inc i) i d)
            (recur (inc i) index max-dist)))))))

;; https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm
(defn douglas-peucker [points epsilon]
  (let [[index max-dist] (max-perpendicular-distance points)]
    (if (> max-dist epsilon)
      (lazy-cat (douglas-peucker (take index points) epsilon)
                (douglas-peucker (drop index points) epsilon))
      [(first points) (last points)])))

(defn simplify-line [line epsilon]
  (gl/linestrip2 (douglas-peucker (:points line) epsilon)))

(defn scene []
  (let [offset (g/translate original (r 0.0 -0.1))
        dampened (g/translate (dampened original 1.0) (r 0.0 0.1))
        simplified (g/translate (simplify-line original 5.0) (r 0.0 0.2))]
    (csvg/svg {:width width :height height :stroke "black" :stroke-width 5.0}
              (svg/polyline (:points original) {:stroke "blue" :key "a1"})
              (svg/polyline (:points offset) {:key "a2"})
              (svg/polyline (:points dampened) {:key "a3"})
              (svg/polyline (:points simplified) {:stroke "red" :key "a4"}))))

(defn page []
  [:div (scene)])

(sketch/definition path-following
  {:created-at "2021-11-12"
   :type :svg
   :tags #{:demo}}
  (ctrl/mount page "canvas-host"))
