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

(defn make-segment [line a b]
  (assoc (gl/line2 a b) :line line))

(defn make-line [line & points]
  (for [[a b] (partition 2 1 points)]
    (make-segment line a b)))

(defn add-line [line segments]
  (loop [base-pos (gv/vec2 (+ 0.01 (* line 0.05)) 0)
         addition []]
    (let [next-pos (tm/+ base-pos (* 0.01 (q/random-gaussian)) (* 0.1 (rand)))
          prov-line (make-segment line base-pos next-pos)]
      (cond (some (fn [s] (geometry/line-intersect s prov-line)) segments)
            (recur base-pos addition)
            (>= (:y next-pos) 1.0)
            (conj addition (make-segment line base-pos
                                         (gv/vec2 (:x next-pos) (min (:y next-pos) 1.0))))
            :else
            (recur next-pos (conj addition prov-line))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:lines [(add-line 0 [])]})

(defn update-state [{:keys [lines] :as state}]
  (let [line-count (count lines)]
    (if (< line-count 20)
      (-> state
          (update :lines conj (add-line line-count (last lines))))
      state)))

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
