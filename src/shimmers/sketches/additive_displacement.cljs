(ns shimmers.sketches.additive-displacement
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

;; Draw consecutive vertical lines from segments, but ensure none of them overlap

(def make-segment gl/line2)

(defn make-line [& points]
  (for [[a b] (partition 2 1 points)]
    (make-segment a b)))

(defn intersects? [a b]
  (#{:intersect} (-> (geom/intersect-line a b) :type)))

(defn retry
  "Retry `retry-fn` until it returns non-nil for up to `tries` attempts."
  [tries retry-fn]
  (let [result (retry-fn)]
    (cond (some? result)
          result
          (= tries 0)
          nil
          :else
          (recur (dec tries) retry-fn))))

(defn find-next [base-pos delta-fn segments]
  (retry 10 #(let [next-pos (tm/+ base-pos (delta-fn))
                   prov-line (make-segment base-pos next-pos)]
               (when-not (some (partial intersects? prov-line) segments)
                 next-pos))))

(defn add-line [segments offset delta-fn]
  (loop [base-pos (tm/+ (-> segments first :points first) offset)
         addition []]
    (when-let [next-pos (find-next base-pos delta-fn segments)]
      (if (>= (:y next-pos) 1.0)
        (conj addition (make-segment base-pos
                                     (gv/vec2 (:x next-pos) (min (:y next-pos) 1.05))))
        (recur next-pos (conj addition (make-segment base-pos next-pos)))))))

(defn delta []
  (fn [] (gv/vec2 (* 0.005 (tm/random -4.0 (rand))) (tm/random 0.02 0.2))))

(defn first-not-nil [xs]
  (first (drop-while nil? xs)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:lines [(first-not-nil (repeatedly (fn [] (add-line [(make-segment (gv/vec2 0 0) (gv/vec2 0 1))]
                                                      (gv/vec2 0.02 0) (delta)))))]})

(defn update-state [{:keys [lines] :as state}]
  (let [previous (last lines)]
    (if (< (-> previous last :points first :x) 1.0)
      (if-let [line (add-line previous (gv/vec2 (* 0.01 (rand)) 0) (delta))]
        (update state :lines conj line)
        state)
      state)))

(defn draw [{:keys [lines]}]
  (q/background 1.0)
  (q/stroke-weight 0.5)
  (doseq [{[a b] :points} (flatten lines)]
    (q/line (cq/rel-pos a) (cq/rel-pos b))))

(sketch/defquil additive-displacement
  :created-at "2021-07-25"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
