(ns shimmers.sketches.hatched-rectangles
  "Reconstruction of https://sighack.com/post/cohen-sutherland-line-clipping-algorithm examples"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.algorithm.line-clipping :as clip]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.sequence :as cs]
            [shimmers.math.deterministic-random :as dr]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.rect :as rect]
            [thi.ng.math.core :as tm]))

(defn nearby [rect rectangles]
  (let [center-x (:x (g/centroid rect))
        width (* 2 (g/width rect))]
    (filter (fn [t] (< (Math/abs (- center-x (:x (g/centroid t)))) width))
            rectangles)))

;; note this only connects edges that completely overlap, it won't form L or T
;; shaped polygons and will only extend an existing rectangle. However hatching
;; only works for rectangles so this works.
(defn neighboring-vertices [rect rectangles]
  (let [edges (g/edges rect)]
    (filter (fn [t]
              (some (fn [[p q]]
                      (some (fn [[p' q']]
                              (or (and (tm/delta= p p') (tm/delta= q q'))
                                  (and (tm/delta= p q') (tm/delta= q p'))))
                            (g/edges t))) edges))
            rectangles)))

;; This may result in overlap as union is *always* a rectangle, doesn't upgrade
;; to polygon.
(defn overlapping-edges [rect rectangles]
  (let [edges (map gl/line2 (g/edges rect))]
    (filter (fn [t]
              (some (fn [line]
                      (some (fn [[p q]]
                              (and (tm/delta= p (g/closest-point line p))
                                   (tm/delta= q (g/closest-point line q))))
                            (g/edges t))) edges))
            rectangles)))

(comment
  (let [rs (g/subdivide (rect/rect 0 0 1 1) {:num 2})]
    (neighboring-vertices (first rs) (rest rs))))

;; TODO: optimize combination steps, probably by more efficiently calculating neighbors
(defn combine [neighboring rectangles percent]
  (let [n (* percent (count rectangles))
        [growth remaining] (split-at n (dr/shuffle rectangles))]
    (loop [growth growth remaining remaining output []]
      (if-let [source (first growth)]
        (let [neighbors (neighboring source (nearby source remaining))]
          (if-let [neighbor (and (seq neighbors) (dr/rand-nth neighbors))]
            (recur (rest growth) (remove #{neighbor} remaining) (conj output (rect/union source neighbor)))
            (recur (rest growth) remaining (conj output source))))
        (into output remaining)))))

(defn noise-angle [rect divisor]
  (let [[x y] (g/centroid rect)]
    (* tm/TWO_PI (q/noise (/ x divisor) (/ y divisor)))))

;; rows/cols is sensitive and causes a freeze, not clear if in hatch-rectangle or clip-lines
(defn setup []
  (q/color-mode :hsl 1.0)
  (q/noise-seed (dr/random-int 1000000))

  (let [sides (dr/weighted {16 1 20 1 24 3 32 2 48 1 64 1})
        depth (dr/weighted {0 0.4 1 0.3 2 0.2 3 0.2 4 0.2 5 0.2})
        algorithm (dr/rand-nth [:vertices :edges])
        combine-with (partial combine ({:vertices neighboring-vertices
                                        :edges overlapping-edges}
                                       algorithm))
        angle (dr/weighted {256 0.4
                            128 0.4
                            64 0.1
                            32 0.2
                            :random 0.4})]
    (println {:sides sides :combine [algorithm depth] :angle angle})
    {:cycles (inc (int (/ (* sides sides) 100)))
     :rectangles (cs/iterate-cycles depth
                                    (fn [rs] (combine-with rs 0.3))
                                    (g/subdivide (cq/screen-rect) {:num sides}))
     :angle (if (= angle :random)
              #(dr/random 0 tm/TWO_PI)
              (fn [r] (noise-angle r angle)))
     :lines []}))

(defn update-state [{:keys [cycles] :as state}]
  (cs/iterate-cycles
   cycles
   (fn [{:keys [rectangles lines angle draw] :as state}]
     (if (empty? rectangles)
       state
       (let [rect (dr/rand-nth rectangles)
             spacing (* (+ 0.5 (- 1.0 (/ (rect/top rect) (q/height))))
                        (dr/random 3.0 9.0))
             theta (angle rect)
             hatches (clip/hatch-rectangle rect spacing theta [(dr/random) (dr/random)])]
         (assoc state
                :rectangles (remove #{rect} rectangles)
                :lines (into lines hatches)
                :draw (into draw hatches)))))
   (assoc state :draw [])))

(defn draw [{:keys [draw]}]
  ;; (q/background 1.0)
  (q/stroke-weight 0.5)
  (q/no-fill)
  (doseq [{[p q] :points} draw]
    (q/line p q)))

(sketch/defquil hatched-rectangles
  :created-at "2021-08-17"
  :size [800 600]
  :tags #{:deterministic}
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
