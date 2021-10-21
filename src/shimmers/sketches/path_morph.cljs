(ns shimmers.sketches.path-morph
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.polygon :as gp]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.utils :as gu]
            [thi.ng.math.core :as tm]
            [shimmers.common.quil :as cq]))

;; TODO: define a path record from a set of points
(defn zig-zag []
  (gp/polygon2 [[0.2 0.2] [0.8 0.2]
                [0.2 0.8] [0.8 0.8]]))

(def shape-sequence
  [[0.0 (g/rotate (gt/triangle2 [0.0 0.0] [1.0 1.0] [0.0 1.0]) 0.5)]
   [0.5 (rect/rect 0.0 0.0 1.0 1.0)]
   [1.0 (gc/circle 0.5)]])

(defn morph [from to t]
  (for [v (range 0.0 1.0 0.03)]
    (tm/mix (g/point-at from v) (g/point-at to v) t)))

(defn shape-at [shapes t]
  (cond (<= t 0.0)
        (let [s (last (first shapes))]
          (morph s s 0.0))
        (>= t 1.0)
        (let [s (last (last shapes))]
          (morph s s 0.0))
        :else
        (let [[before after] (split-with (fn [[time _]] (< time t)) shapes)
              [t0 from] (last before)
              [t1 to] (first after)]
          (morph from to (/ (- t t0) (- t1 t0))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:path (zig-zag)
   :t 0.0})

(defn update-state [state]
  (update state :t + 0.0025))

(defn draw [{:keys [path t]}]
  ;; (q/background 1.0 0.1)
  (q/no-fill)
  (q/stroke-weight 0.5)
  (q/stroke 0.0 0.5)
  (let [bounds (rect/rect 0 0 (q/width) (q/height))
        t (mod t 1.0)
        p (g/unmap-point bounds (gu/point-at t (:points path)))
        shape (gp/polygon2 (shape-at shape-sequence t))]
    ;; (cq/circle p 1)
    (cq/draw-shape (g/vertices (g/translate (g/scale-size shape 50.0) p)))))

(sketch/defquil path-morph
  :created-at "2021-10-06"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
