(ns shimmers.sketches.additive-displacement
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.geometry :as geometry]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

;; Draw line vertical lines from segments, but ensure none of them overlap

(defn make-segment [a b]
  (gl/line2 a b))

(defn make-line [& points]
  (for [[a b] (partition 2 1 points)]
    (make-segment a b)))

(defn add-line [segments]
  (loop [base-pos (tm/+ (-> segments first :points first) (gv/vec2 (* 0.02 (rand)) 0))
         addition []]
    (let [next-pos (tm/+ base-pos (* 0.003 (q/random-gaussian)) (* 0.08 (rand)))
          prov-line (make-segment base-pos next-pos)]
      (cond (some (fn [s] (geometry/line-intersect s prov-line)) segments)
            (recur base-pos addition)
            (>= (:y next-pos) 1.0)
            (conj addition (make-segment base-pos
                                         (gv/vec2 (:x next-pos) (min (:y next-pos) 1.0))))
            :else
            (recur next-pos (conj addition prov-line))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:lines [(add-line [(make-segment (gv/vec2 0 0) (gv/vec2 0 1))])]})

(defn update-state [{:keys [lines] :as state}]
  (if (< (count lines) 50)
    (update state :lines conj (add-line (last lines)))
    state))

(defn draw [{:keys [lines]}]
  (q/background 1.0)
  (q/stroke-weight 1.5)
  (doseq [{[a b] :points} (flatten lines)]
    (q/line (cq/rel-pos a) (cq/rel-pos b))))

(sketch/defquil additive-displacement
  :created-at "2021-07-25"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
