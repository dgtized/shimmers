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

(defn left [size]
  (gv/vec2 (- size) 0))

(defn right [size]
  (gv/vec2 size 0))

(defn forward [size]
  (gv/vec2 0 size))

(defn left-diag [size]
  (tm/+ (left size) (forward size)))

(defn right-diag [size]
  (tm/+ (right size) (forward size)))

(defn next-pos [start size l p]
  (let [h-dist (horizontal-dist p start)
        right-bound? (< h-dist size)
        left-bound? (> h-dist (- size))
        d (dr/weighted {(left size) (if right-bound? 1 0)
                        (right size) (if left-bound?  1 0)
                        (forward size) 1
                        (right-diag size) (if left-bound? 2 0)
                        (left-diag size) (if right-bound?  2 0)
                        (right-diag (* 2 size)) (if (tm/delta= h-dist size) 1 0)
                        (left-diag (* 2 size)) (if (tm/delta= h-dist (- size)) 1 0)})
        p' (tm/+ p d)]
    (if-not (tm/delta= l p')
      p'
      (recur start size l p))))

(defn deco-path [start end size]
  (let [start' (tm/+ start (forward size))
        end' (tm/- end (forward size))
        steps (->> [start start']
                   (iterate (fn [[l p]] (let [p' (next-pos start size l p)]
                                         [p p'
                                          (if (or (> (tm/mag p') (* 2 size))
                                                  (and (> (tm/mag p') size) (dr/chance 0.5)))
                                            [:Q (gv/vec2 (:x p') (:y p)) p']
                                            [:L p'])])))
                   rest
                   (take-while (fn [[_ p _]] (>= (vertical-dist p end) size)))
                   (mapv (fn [[_ _ cmd]] cmd)))]
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
     (shapes 7))))

(sketch/definition deco-screens
  {:created-at "2023-02-13"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :deco-screens)
              "sketch-host"))
