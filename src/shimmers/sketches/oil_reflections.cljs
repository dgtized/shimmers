(ns shimmers.sketches.oil-reflections
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.geometry :as geometry]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]))

(defn intersects [c1 c2]
  (when (geometry/circles-overlap? c1 c2)
    c2))

(defn add-circle [circles bounds r]
  (let [p (geom/random-point-inside bounds)
        near circles ;; todo optimize
        candidate (gc/circle p r)]
    (when-not (some (partial intersects candidate) near)
      candidate)))

(defn circle-pack [circles bounds radius n-candidates]
  (loop [i 0 circles circles]
    (if (>= i n-candidates)
      circles
      (if-let [circle (add-circle circles bounds radius)]
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
                     :else 8.0)]
    (if (>= n 64)
      state
      (update state :circles circle-pack bounds radius 10))))

(defn draw [{:keys [circles]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (doseq [{:keys [p r]} circles]
    (cq/circle p r)))

(sketch/defquil oil-reflections
  :created-at "2021-09-05"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
