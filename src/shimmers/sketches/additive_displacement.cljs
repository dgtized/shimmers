(ns shimmers.sketches.additive-displacement
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.sequence :as cs]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

;; Draw consecutive vertical lines from segments, but ensure none of them overlap

(def make-segment gl/line2)

;; https://stackoverflow.com/questions/1073336/circle-line-segment-collision-detection-algorithm
;; https://www.scratchapixel.com/lessons/3d-basic-rendering/minimal-ray-tracer-rendering-simple-shapes/ray-sphere-intersection
(defn circle-segment-intersection
  [{:keys [c r]} {[p q] :points}]
  (let [d (tm/- q p)
        f (tm/- p c)
        a (tm/dot d d)
        b (* 2 (tm/dot f d))
        c (- (tm/dot f f) (* r r))
        discriminant (- (* b b) (* 4 a c))]
    (when (>= discriminant 0)
      (if (> discriminant 0)
        (let [root-disc (Math/sqrt discriminant)
              reciprocal (/ 1 (* 2 a))
              t1 (* (- (- b) root-disc) reciprocal)
              t2 (* (+ (- b) root-disc) reciprocal)]
          [discriminant t1 t2 (tm/+ p (tm/* d t1)) (tm/+ p (tm/* d t2))])
        [discriminant (/ (- b) (* 2 a))]))))

(comment (circle-segment-intersection (gc/circle [1 1] 1) (gl/line2 [1 1] [2 2]))
         (circle-segment-intersection (gc/circle [1 1] 1) (gl/line2 [1 1] [2 0]))
         (circle-segment-intersection (gc/circle [2 0] 1) (gl/line2 [0 0] [5 0]))
         (circle-segment-intersection (gc/circle [0 0] 1) (gl/line2 [1 0] [2 0]))
         (circle-segment-intersection (gc/circle [0 0] 10) (gl/line2 [1 1] [2 1]))
         )

(defn intersects? [a b]
  (#{:intersect} (-> (geom/intersect-line a b) :type)))

(defn find-next [base-pos delta-fn segments avoid]
  (let [next-pos (tm/+ base-pos (delta-fn))
        prov-line (make-segment base-pos next-pos)]
    (when-not (or (some (fn [{:keys [p r]}] (< (geom/dist (cq/rel-vec next-pos) p) r)) avoid)
                  (some (partial intersects? prov-line) segments))
      next-pos)))

(defn add-line [segments offset delta-fn avoid]
  (loop [base-pos (tm/+ (-> segments first :points first) offset)
         addition []]
    (when-let [next-pos (cs/retry 10 #(find-next base-pos delta-fn segments avoid))]
      (if (>= (:y next-pos) 1.0)
        (conj addition (make-segment base-pos
                                     (gv/vec2 (:x next-pos) (min (:y next-pos) 1.05))))
        (recur next-pos (conj addition (make-segment base-pos next-pos)))))))

(defn delta []
  (fn [] (gv/vec2 (* 0.01 (tm/random -4.0 1.0)) (tm/random 0.05 0.15))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [avoid (repeatedly 12 #(gc/circle (cq/rel-pos (rand) (rand)) (tm/random 5 10)))
        first-line (cs/retry 10 #(add-line [(make-segment (gv/vec2 0 0) (gv/vec2 0 1))]
                                           (gv/vec2 0.02 0) (delta)
                                           avoid))]
    {:avoid avoid
     :lines [first-line]}))

(defn update-state [{:keys [lines avoid] :as state}]
  (let [previous (last lines)]
    (if (< (-> previous last :points first :x) 1.0)
      (if-let [line (add-line previous (gv/vec2 (* 0.015 (rand)) 0) (delta) avoid)]
        (update state :lines conj line)
        state)
      state)))

(defn draw [{:keys [lines avoid]}]
  (q/background 1.0)
  (q/stroke-weight 0.5)
  (q/ellipse-mode :radius)
  (doseq [{:keys [p r]} avoid]
    (cq/circle p r))
  (doseq [{[a b] :points} (flatten lines)]
    (q/line (cq/rel-pos a) (cq/rel-pos b))))

(sketch/defquil additive-displacement
  :created-at "2021-07-25"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
