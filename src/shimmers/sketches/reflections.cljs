(ns shimmers.sketches.reflections
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.types :refer [Circle2]]
   [thi.ng.geom.vector :as gv]))

(defn reflect-identity [_ p]
  p)

(defn reflect-x [{[width _height] :size} [x y]]
  (gv/vec2 (+ width (- width x)) y))

(defn reflect-y [{[_width height] :size} [x y]]
  (gv/vec2 x (+ height (- height y))))

(defn reflect-xy [box p]
  (reflect-y box (reflect-x box p)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:shapes [(gc/circle 0.6 0.4 0.2)
            (gc/circle 0.4 0.3 0.1)
            (gl/line2 (gv/vec2 0.1 0.1) (gv/vec2 0.9 0.1))
            (gl/line2 (gv/vec2 0.1 0.5) (gv/vec2 0.9 0.9))]})

(defn update-state [state]
  state)

(defn draw [{:keys [shapes]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (let [height (q/height)
        width (q/width)
        box (rect/rect 0 0 (/ width 2) (/ height 2))
        h (g/height box)]
    (doseq [s shapes]
      (cond (instance? Circle2 s)
            (let [{:keys [p r]} s]
              (doseq [point ((juxt reflect-identity reflect-x reflect-y reflect-xy)
                             box (g/unmap-point box p))]
                (cq/circle point (* r h))))
            :else
            (doseq [view [reflect-identity reflect-x reflect-y reflect-xy]]
              (q/begin-shape)
              (doseq [v (g/vertices s)]
                (apply q/vertex (view box (g/unmap-point box v))))
              (q/end-shape))))))

(sketch/defquil reflections
  :created-at "2023-01-16"
  :tags #{:genuary2023}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
