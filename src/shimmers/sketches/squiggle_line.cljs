(ns shimmers.sketches.squiggle-line
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.algorithm.hand-drawn :as hand-drawn]
   [thi.ng.geom.line :as gl]))

(defn rel-line [{[p q] :points}]
  (gl/line2 (cq/rel-vec p) (cq/rel-vec q)))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/no-loop)
  {:lines (mapv rel-line [(gl/line2 0.2 0.2 0.8 0.2)
                          (gl/line2 0.3 0.3 0.6 0.3)
                          (gl/line2 0.7 0.25 0.7 0.6)
                          (gl/line2 0.8 0.35 0.8 0.8)
                          (gl/line2 0.2 0.7 0.5 0.4)
                          (gl/line2 0.5 0.5 0.6 0.7)])})

(defn update-state [state]
  state)

(defn draw [{:keys [lines]}]
  (q/background 1.0)
  (doseq [{[p q] :points} lines]
    (hand-drawn/line p q)))

(sketch/defquil squiggle-line
  :created-at "2021-11-05"
  :tags #{:demo}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
