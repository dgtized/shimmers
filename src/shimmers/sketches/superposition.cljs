(ns shimmers.sketches.superposition
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]))

(defn rel-h [p]
  (* (q/height) p))

(defn rel-w [p]
  (* (q/width) p))

(defn draw-polygon [poly]
  (cq/draw-shape (geom/vertices poly)))

(defn setup []
  {:shapes [(rect/rect (rel-w 0.15) (rel-h 0.15) (rel-w 0.3) (rel-h 0.3))
            (rect/rect (rel-w 0.55) (rel-h 0.55) (rel-w 0.3) (rel-h 0.3))]})

(defn update-state [state]
  state)

(defn draw [{:keys [shapes]}]
  (doseq [shape shapes]
    (draw-polygon shape)))

(defn ^:export run-sketch []
  ;; 20210308
  (q/defsketch superposition
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
