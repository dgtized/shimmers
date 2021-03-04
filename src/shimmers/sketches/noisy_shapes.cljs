(ns shimmers.sketches.noisy-shapes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.polygon :as poly]
            [thi.ng.geom.rect :as rect]))

(defn setup []
  (let [w (q/width)
        h (q/height)]
    {:shape (rect/rect (* 0.2 w) (* 0.2 h) (* 0.5 w) (* 0.6 h))}))

(defn update-state [state]
  state)

(defn draw-polygon [poly]
  (q/begin-shape)
  (doseq [p (geom/vertices poly)]
    (apply q/vertex p))
  (q/end-shape :close))

(defn draw [{:keys [shape]}]
  (let [poly (geom/as-polygon shape)]
    (draw-polygon poly)))

(defn ^:export run-sketch []
  (q/defsketch noisy-shapes
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
