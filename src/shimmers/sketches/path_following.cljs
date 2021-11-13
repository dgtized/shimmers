(ns shimmers.sketches.path-following
  (:require
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
      (g/translate (r 0.0 0.05))))

(defn dampened [{path :points} factor]
  (gl/linestrip2
   (let [a (first path)
         b (last path)]
     (mapv (fn [pt t] (tm/mix pt (tm/mix a b t) factor))
           path
           (tm/norm-range (dec (count path)))))))

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
  (csvg/svg {:width width :height height}
            (concat [(svg/polyline (:points original)
                                   {:stroke "#efc020"
                                    :stroke-width 10.0
                                    :key "original"})
                     (let [factors [1.0 2.0 4.0 8.0 12.0 14.0]]
                       (for [[i eps] (map-indexed vector factors)]
                         (svg/polyline (:points (g/translate (simplify-line original eps)
                                                             (r 0.0 (- -0.07 (* 0.05 i)))))
                                       {:stroke "#da3b29"
                                        :stroke-width (* 3.0 (- 1.0 (/ i (count factors))))
                                        :key (str "s" i)})))
                     (for [v (range 0.0 1.0 0.1)]
                       (svg/polyline (:points (g/translate (dampened original v)
                                                           (r 0.0 (+ 0.07 (* 0.3 v)))))
                                     {:stroke "#3a3421"
                                      :stroke-width (* 3.0 (- 1.0 v))
                                      :key (str "a" v)}))])))

(defn page []
  [:div (scene)])

(sketch/definition path-following
  {:created-at "2021-11-12"
   :type :svg
   :tags #{:demo}}
  (ctrl/mount page "canvas-host"))
