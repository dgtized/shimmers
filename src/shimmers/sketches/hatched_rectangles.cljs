(ns shimmers.sketches.hatched-rectangles
  "Reconstruction of https://sighack.com/post/cohen-sutherland-line-clipping-algorithm examples"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.algorithm.line-clipping :as clip]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.rect :as rect]
            [thi.ng.math.core :as tm]))

;; note this only connects edges that completely overlap, it won't form L or T
;; shaped polygons and will only extend an existing rectangle. However hatching
;; only works for rectangles so this works.
(defn neighboring-vertices [rect rectangles]
  (let [edges (geom/edges rect)]
    (filter (fn [t]
              (some (fn [[p q]]
                      (some (fn [[p' q']]
                              (or (and (tm/delta= p p') (tm/delta= q q'))
                                  (and (tm/delta= p q') (tm/delta= q p'))))
                            (geom/edges t))) edges))
            rectangles)))

;; This may result in overlap as union is *always* a rectangle, doesn't upgrade
;; to polygon.
(defn overlapping-edges [rect rectangles]
  (let [edges (map gl/line2 (geom/edges rect))]
    (filter (fn [t]
              (some (fn [line]
                      (some (fn [[p q]]
                              (and (tm/delta= p (geom/closest-point line p))
                                   (tm/delta= q (geom/closest-point line q))))
                            (geom/edges t))) edges))
            rectangles)))

(comment
  (let [rs (geom/subdivide (rect/rect 0 0 1 1) {:num 2})]
    (neighboring-vertices (first rs) (rest rs))))

(defn combine [neighboring rectangles percent]
  (let [n (* percent (count rectangles))
        [growth remaining] (split-at n (shuffle rectangles))]
    (loop [growth growth remaining remaining output []]
      (if-let [source (first growth)]
        (let [neighbors (neighboring source remaining)]
          (if-let [neighbor (and (seq neighbors) (rand-nth neighbors))]
            (recur (rest growth) (remove #{neighbor} remaining) (conj output (rect/union source neighbor)))
            (recur (rest growth) remaining (conj output source))))
        (into output remaining)))))

;; rows/cols is sensitive and causes a freeze, not clear if in hatch-rectangle or clip-lines
(defn setup []
  (q/color-mode :hsl 1.0)
  (let [combine-with (partial combine (rand-nth [neighboring-vertices overlapping-edges]))]
    {:rectangles (-> (rect/rect (cq/rel-pos 0 0) (cq/rel-pos 1.0 1.0))
                     (geom/subdivide {:num 24})
                     (combine-with 0.4)
                     (combine-with 0.4)
                     (combine-with 0.2)
                     (combine-with 0.2))
     :lines []}))

(defn noise-angle [rect divisor]
  (let [[x y] (geom/centroid rect)]
    (* tm/TWO_PI (q/noise (/ x divisor) (/ y divisor)))))

(defn update-state [{:keys [rectangles lines] :as state}]
  (if (empty? rectangles)
    state
    (let [rect (rand-nth rectangles)
          spacing (* (+ 0.5 (- 1.0 (/ (rect/top rect) (q/height))))
                     (tm/random 3.0 9.0))
          theta (noise-angle rect 128) ;; (tm/random 0 tm/TWO_PI)
          hatches (clip/hatch-rectangle rect spacing theta)]
      (assoc state
             :rectangles (remove #{rect} rectangles)
             :lines (into lines hatches)
             :draw hatches))))

(defn draw [{:keys [draw]}]
  ;; (q/background 1.0)
  (q/stroke-weight 0.5)
  (q/no-fill)
  (doseq [{[p q] :points} draw]
    (q/line p q)))

(sketch/defquil hatched-rectangles
  :created-at "2021-08-17"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
