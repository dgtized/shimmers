(ns shimmers.sketches.additive-displacement
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.sequence :as cs]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; Draw consecutive vertical lines from segments, but ensure none of them overlap

(def make-segment gl/line2)

(defn intersects? [a b]
  (#{:intersect} (-> (g/intersect-line a b) :type)))

(defn find-next [base-pos delta-fn segments avoid]
  (let [next-pos (tm/+ base-pos (delta-fn))
        prov-line (make-segment base-pos next-pos)]
    (when-not (or (some (fn [{:keys [p r]}] (< (g/dist (cq/rel-vec next-pos) p) r)) avoid)
                  (some (partial intersects? prov-line) segments))
      next-pos)))

(defn add-line [points offset delta-fn avoid]
  (let [base-pos (tm/+ (first points) offset)
        segments (lines/points->lines points)]
    (loop [prev-pos base-pos
           addition [base-pos]]
      (when-let [next-pos (cs/retry 10 #(find-next prev-pos delta-fn segments avoid))]
        (if (>= (:y next-pos) 1.0)
          (conj addition (gv/vec2 (:x next-pos) (min (:y next-pos) 1.05)))
          (recur next-pos (conj addition next-pos)))))))

(defn delta []
  (fn [] (gv/vec2 (* 0.01 (tm/random -4.0 1.0)) (tm/random 0.05 0.15))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:avoid (repeatedly 12 #(gc/circle (cq/rel-pos (rand) (rand)) (tm/random 5 10)))
   :lines [[(gv/vec2 0 0) (gv/vec2 0 1)]]})

(defn update-state [{:keys [lines avoid] :as state}]
  (let [previous (last lines)]
    (if (< (-> previous last :x) 1.0)
      (if-let [points (add-line previous (gv/vec2 (* 0.015 (rand)) 0) (delta) avoid)]
        (update state :lines conj points)
        state)
      state)))

(defn draw [{:keys [lines avoid]}]
  (q/background 1.0)
  (q/stroke-weight 0.5)
  (q/ellipse-mode :radius)
  (q/no-fill)
  (doseq [c avoid]
    (cq/circle c))
  (doseq [line lines]
    (cq/draw-path (map cq/rel-pos line))))

(sketch/defquil additive-displacement
  :created-at "2021-07-25"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
