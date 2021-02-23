(ns shimmers.sketches.triangulating-subdivisions
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.vector :as v]
            [thi.ng.geom.core :as geom]))

(defrecord Line [a b])

(defn subdivide [line]
  (let [{:keys [a b]} line
        cutpoint (v/scale (v/sub a b) (rand))
        midpoint (v/add b cutpoint)
        normal (geom/scale (geom/normal cutpoint) 0.1)]
    [(->Line a midpoint)
     (->Line midpoint b)
     (->Line (v/sub midpoint normal)
             (v/add midpoint normal))]))

(defn setup []
  (let [top (v/vec2 (* 0.5 (q/width)) (* 0.1 (q/height)))
        left (v/vec2 (* 0.1 (q/width)) (* 0.9 (q/height)))
        right (v/vec2 (* 0.9 (q/width)) (* 0.9 (q/height)))]
    {:lines (mapcat subdivide
                    [(->Line top left)
                     (->Line left right)
                     (->Line right top)])}))

(defn update-state [state]
  state)

(defn draw [{:keys [lines]}]
  (doseq [line lines]
    (q/line (:a line) (:b line))))

(defn ^:export run-sketch []
  (q/defsketch triangulating-subdivisions
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
