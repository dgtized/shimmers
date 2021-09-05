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
            [thi.ng.geom.rect :as rect]
            [thi.ng.math.core :as tm]))

(defn intersects
  [spacing
   {p1 :p r1 :r}
   {p2 :p r2 :r :as c2}]
  (let [dist-sqr (eq/sq (+ r1 r2 spacing))]
    (when (< (geom/dist-squared p1 p2) dist-sqr)
      c2)))

(defn add-circle [circles bounds radius spacing]
  (let [p (geom/random-point-inside bounds)
        near circles ;; todo optimize
        candidate (gc/circle p radius)]
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

(defn remove-middle [hatches n]
  (let [c (count hatches)
        sorted (sort-by (fn [{[[x y] _] :points}] [y x]) hatches)
        edges (int (- (/ c 2) (/ n 2)))]
    (into (take edges sorted)
          (take-last edges sorted))))

(defn reflect-hatching [{[x _] :p r :r :as c}]
  (-> c
      (clip/hatch-circle
       (tm/clamp (/ r 8.0) 3.0 8.0)
       (p/gaussian 5.8 (* 0.2 (/ x (q/width)))))
      (remove-middle (inc (rand-int (int (/ r 6)))))))

(defn update-state [{:keys [bounds circles] :as state}]
  (let [n (count circles)
        radius (cond (<= n 6) 48.0
                     (<= n 16.0) 32.0
                     (<= n 24) 24.0
                     (<= n 48) 12.0
                     (<= n 64) 8.0
                     :else 6.0)]
    (if (>= n 160)
      state
      (-> state
          (update :circles circle-pack bounds radius radius 10)
          (update :circles (partial p/map-random-sample
                                    (fn [{[_ y] :p}] (eq/gaussian 0.05 (/ (q/height) 2) (/ (q/height) 8) y))
                                    (fn [c] (assoc c :hatching (reflect-hatching c)))))))))

(defn draw [{:keys [circles]}]
  (q/background 1.0)
  (q/stroke-weight 0.7)
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
