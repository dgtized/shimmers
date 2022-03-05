(ns shimmers.sketches.vanishing-point
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.vector :as gv]
            [shimmers.common.ui.debug :as debug]
            [shimmers.math.vector :as v]
            [thi.ng.geom.core :as g]))

(defonce defo (debug/state))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:bounds (cq/screen-rect)
   :mouse (gv/vec2)})

(defn update-state [{:keys [bounds] :as state}]
  (assoc state :mouse (v/clamp-bounds bounds (cq/mouse-position))))

(defn draw [{:keys [bounds mouse] :as state}]
  (q/background 1.0 0.05)
  (reset! defo state)
  (doseq [p (g/vertices bounds)]
    (q/line p mouse)))

(sketch/defquil template
  :created-at "2022-03-05"
  :on-mount (fn [] (debug/mount defo))
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
