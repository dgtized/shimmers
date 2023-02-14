(ns shimmers.sketches.deco-screens
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn horizontal-dist [[x0 _] [x1 _]]
  (- x1 x0))

(defn vertical-dist [[_ y0] [_ y1]]
  (- y1 y0))

(defn next-pos [start size l p]
  (let [right-bound? (< (horizontal-dist p start) size)
        left-bound? (> (horizontal-dist p start) (- size))
        d (dr/weighted {(gv/vec2 size 0.0) (if right-bound? 1 0)
                        (gv/vec2 (- size 0.0) 0.0) (if left-bound?  1 0)
                        (gv/vec2 0.0 size) 1
                        (gv/vec2 size size) (if left-bound? 1 0)
                        (gv/vec2 (- size) size) (if right-bound?  1 0)})
        p' (tm/+ p d)]
    (if-not (tm/delta= l p')
      p'
      (recur start size l p))))

(defn deco-path [start end size]
  (let [start' (tm/+ start (gv/vec2 0.0 size))
        end' (tm/- end (gv/vec2 0.0 size))
        steps (->> [start start']
                   (iterate (fn [[l p]] [p (next-pos start size l p)]))
                   rest
                   (take-while (fn [[_ p]] (> (vertical-dist p end) size)))
                   (mapv (fn [[_ p]] [:L p])))]
    (->> (concat [[:M start]
                  [:L start']]
                 steps
                 [[:L end']
                  [:L end]])
         csvg/path)))

(defn shapes [n]
  (mapv (fn [i]
          (let [s (/ 1.0 (inc n))
                x (* s (inc i))]
            (deco-path (rv x 0.0) (rv x 1.0) (/ height (* 2 (inc n))))))
        (range n)))

(defn scene []
  (csvg/timed
   (csvg/svg {:width width
              :height height
              :stroke "black"
              :fill "none"
              :stroke-width 2.0}
     (shapes 5))))

(sketch/definition deco-screens
  {:created-at "2023-02-13"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :deco-screens)
              "sketch-host"))
