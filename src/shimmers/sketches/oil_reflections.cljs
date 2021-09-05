(ns shimmers.sketches.oil-reflections
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.algorithm.line-clipping :as clip]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.equations :as eq]
            [shimmers.math.probability :as p]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]))

(defn intersects
  [spacing
   {p1 :p r1 :r}
   {p2 :p r2 :r :as c2}]
  (let [dist-sqr (eq/sq (+ r1 r2 spacing))]
    (when (< (geom/dist-squared p1 p2) dist-sqr)
      c2)))

(defn add-circle [circles bounds r spacing]
  (let [p (geom/random-point-inside bounds)
        near circles ;; todo optimize
        candidate (gc/circle p r)]
    (when-not (some (partial intersects spacing candidate) near)
      candidate)))

(defn circle-pack [circles bounds radius spacing n-candidates]
  (loop [i 0 circles circles]
    (if (>= i n-candidates)
      circles
      (if-let [circle (add-circle circles bounds radius spacing)]
        (recur (inc i)
               (conj circles circle))
        (recur (inc i) circles)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:bounds (rect/rect (cq/rel-pos 0.1 0.1) (cq/rel-pos 0.9 0.9))
   :circles []})

(defn update-state [{:keys [bounds circles] :as state}]
  (let [n (count circles)
        radius (cond (<= n 6) 48.0
                     (<= n 16.0) 32.0
                     (<= n 24) 24.0
                     (<= n 48) 12.0
                     (<= n 64) 8.0
                     :else 6.0)]
    (if (>= n 128)
      state
      (-> state
          (update :circles circle-pack bounds radius radius 10)
          (update :circles (partial p/map-random-sample (constantly 0.02)
                                    (fn [c] (assoc c :hatching (clip/hatch-circle c (/ (:r c) 3.0) 5.8)))))))))

(defn draw [{:keys [circles]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (doseq [{:keys [p r hatching]} circles]
    (cq/circle p r)
    (when (seq hatching)
      (doseq [{[p q] :points} hatching]
        (q/line p q)))))

(sketch/defquil oil-reflections
  :created-at "2021-09-05"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
