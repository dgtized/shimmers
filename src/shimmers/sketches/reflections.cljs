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

(defn reflect-identity [box p]
  (g/unmap-point box p))

(defn reflect-x [box [x y]]
  (g/unmap-point box (gv/vec2 (- 1.0 x) y)))

(defn reflect-y [box [x y]]
  (g/unmap-point box (gv/vec2 x (- 1.0 y))))

(defn reflect-xy [box [x y]]
  (g/unmap-point box (gv/vec2 (- 1.0 x) (- 1.0 y))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0
   :shapes [(gc/circle 0.6 0.4 0.2)
            (gc/circle 0.4 0.3 0.1)
            (gl/line2 (gv/vec2 0.1 0.1) (gv/vec2 0.9 0.1))
            (gl/line2 (gv/vec2 0.1 0.5) (gv/vec2 0.9 0.9))]})

(defn update-state [state]
  (update state :t + 0.005))

(defn draw [{:keys [t shapes]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (let [height (q/height)
        width (q/width)
        circle (gc/circle (cq/rel-vec 0.5 0.5) (* height 0.2))
        [rx ry] (g/point-at circle t)
        box (rect/rect 0 0 rx ry)
        reflections [[reflect-identity box]
                     [reflect-x (rect/rect rx 0 (- width rx) ry)]
                     [reflect-y (rect/rect 0 ry rx (- height ry))]
                     [reflect-xy (rect/rect rx ry (- width rx) (- height ry))]]]
    (doseq [s shapes]
      (cond (instance? Circle2 s)
            (let [{:keys [p r]} s]
              (doseq [[reflect box] reflections]
                (cq/circle (reflect box p) (* r (g/height box)))))
            :else
            (doseq [[reflect box] reflections]
              (q/begin-shape)
              (doseq [v (g/vertices s)]
                (apply q/vertex (reflect box v)))
              (q/end-shape))))))

(sketch/defquil reflections
  :created-at "2023-01-16"
  :tags #{:genuary2023}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
