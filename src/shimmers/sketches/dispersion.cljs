(ns shimmers.sketches.dispersion
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.rect :as rect]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [shimmers.math.probability :as p]
            [thi.ng.geom.triangle :as gt]))



(defn draw-polygon [poly]
  (cq/draw-shape (geom/vertices poly)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn draw [_]
  (q/background 1.0)
  (q/stroke-weight 0.2)
  (let [building (-> (rect/rect (cq/rel-w 0.15) (cq/rel-h 0.6) (cq/rel-w 0.15) (cq/rel-h 0.4))
                     (geom/tessellate {:num 24}))]
    (doseq [shape (mapcat (fn [s] (if (p/chance 0.1) (geom/subdivide s) [s])) building)]
      (draw-polygon shape)))

  (doseq [s (geom/subdivide (gt/triangle2 [0 0] [0 50] [50 0]))]
    (draw-polygon s)))

(defn ^:export run-sketch []
  (q/defsketch dispersion
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
