(ns shimmers.sketches.geometry-interactive
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.debug :as debug]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [shimmers.algorithm.square-packing :as square]))

(defonce defo (debug/state {}))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:position (gv/vec2)})

(defn update-state [state]
  (update state :position cq/mouse-last-position-clicked))

(defn draw [{:keys [position]}]
  (q/background 1.0)
  (q/no-fill)
  (q/stroke 0.0)
  (let [a (cq/screen-rect 0.4)
        b (assoc (cq/screen-rect 0.3) :p position)
        diffs (square/difference a b)]
    (qdg/draw a)
    (qdg/draw b)
    (q/stroke 0.0 0.5 0.5)
    (q/fill 0.0 0.1)
    (doseq [s diffs]
      (qdg/draw s))
    (swap! defo assoc :differences diffs)))

(sketch/defquil geometry-interactive
  :created-at "2023-02-16"
  :tags #{}
  :on-mount (debug/mount defo)
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
