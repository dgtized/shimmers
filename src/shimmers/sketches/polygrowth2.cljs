(ns shimmers.sketches.polygrowth2
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.color :as color]
            [shimmers.math.probability :as p]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.polygon :as gp]
            [thi.ng.geom.rect :as rect]
            [thi.ng.math.core :as tm]))

(defn inside-another? [shapes point]
  (some (fn [s] (geom/contains-point? s point)) shapes))

(defn as-polygon [[shape color]]
  (with-meta (geom/as-polygon shape) {:stroke color}))

(defn grow-vertices [bounds shapes p factor polygon]
  (let [center (geom/centroid polygon)]
    (as-> polygon it
      (geom/vertices it)
      (p/map-random-sample
       (constantly p)
       (fn [v] (let [v' (tm/+ center (tm/* (tm/- v center) factor))]
                (cond (inside-another? shapes v')
                      v
                      (not (inside-another? bounds v'))
                      v
                      :else v'))) it)
      (gp/polygon2 it)
      (with-meta it (meta polygon)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:bounds [(geom/scale-size (rect/rect 0 0 (q/width) (q/height)) 1.2)]
   :shapes (map as-polygon
                {(gc/circle (cq/rel-pos 0.35 0.65) 40) (color/hex->hsla "#3b4d61")
                 (gc/circle (cq/rel-pos 0.2 0.3) 30) (color/hex->hsla "#ef9d10")
                 (gc/circle (cq/rel-pos 0.75 0.4) 20) (color/hex->hsla "#3b4d61" 2.0)})})

(defn update-state [state]
  (update state :shapes (partial map (partial grow-vertices
                                              (:bounds state)
                                              (:shapes state)
                                              0.05
                                              1.05))))

(defn draw [{:keys [shapes]}]
  (q/ellipse-mode :radius)
  (q/stroke-weight 0.05)
  (doseq [shape shapes
          :let [vertices (geom/vertices shape)]]
    (q/no-fill)
    (q/stroke 0)
    (when-let [color (:stroke (meta shape))]
      (apply q/stroke color))
    (cq/draw-shape vertices)
    (q/fill 0)
    (doseq [[x y] vertices]
      (q/ellipse x y 0.3 0.3))))

(defn ^:export run-sketch []
  ;; 20210502
  (q/defsketch polygrowth2
    :host "quil-host"
    :size [900 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
