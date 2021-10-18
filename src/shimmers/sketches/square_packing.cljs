(ns shimmers.sketches.square-packing
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as geom]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [shimmers.math.probability :as p]))

(def PHI (/ (+ 1 (Math/sqrt 5)) 2))

(defn split-x [{:keys [p size]} square]
  (let [[width height] size]
    [(rect/rect (tm/+ p (gv/vec2 square 0)) (- width square) height)
     (rect/rect (tm/+ p (gv/vec2 0 square)) square (- height square))]))

(defn split-y [{:keys [p size]} square]
  (let [[width height] size]
    [(rect/rect (tm/+ p (gv/vec2 square 0)) (- width square) square)
     (rect/rect (tm/+ p (gv/vec2 0 square)) width (- height square))]))

(defn pack [rectangle]
  (let [{:keys [p size]} rectangle
        [w h] size
        min-side (min w h)
        side (* min-side (/ 1 PHI))
        split (p/weighted {split-x w
                           split-y h})]
    [(rect/rect p side side)
     (split rectangle side)]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:squares []
   :remaining [(rect/rect [10 10] [790 590])]})

(defn update-state [{:keys [remaining] :as state}]
  (if (< (count remaining) 64)
    (let [rect (p/weighted-by geom/area remaining)
          [s r] (pack rect)]
      (-> state
          (assoc :remaining (into (remove #{rect} remaining) r))
          (update :squares conj s)))
    state))

(defn draw [{:keys [squares remaining]}]
  (q/background 1.0)
  (q/stroke 0.35 0.5 0.5 1.0)
  (q/fill 1.0 1.0)
  (doseq [rects remaining]
    (cq/draw-shape (geom/vertices rects)))
  (q/stroke 0.0 0.0 0.0 1.0)
  (q/fill 1.0 0.1)
  (doseq [square squares]
    (cq/draw-shape (geom/vertices square))))

(sketch/defquil square-packing
  :created-at "2021-10-17"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
