(ns shimmers.sketches.polygrowth
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.polygon :as gp]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.math.core :as tm]))

(defn inside-another? [shapes point]
  (some (fn [s] (geom/contains-point? s point)) shapes))

(defn grow-clipped [bounds shapes factor polygon]
  (let [center (geom/centroid polygon)]
    (-> (for [v (geom/vertices polygon)]
          (let [v' (tm/+ center (tm/* (tm/- v center) factor))]
            (cond (inside-another? shapes v')
                  v
                  (not (inside-another? bounds v'))
                  v
                  :else v')))
        gp/polygon2
        (with-meta (meta polygon)))))

(defn as-polygon [shape]
  (with-meta (geom/as-polygon shape) (meta shape)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:bounds [(geom/scale-size (rect/rect 0 0 (q/width) (q/height)) 4)]
   :shapes (map as-polygon
                [(with-meta (gc/circle (cq/rel-pos 0.4 0.5) 40) {:stroke [0.0 0.5 0.35 1.0]})
                 (with-meta (gc/circle (cq/rel-pos 0.2 0.3) 30) {:stroke [0.5 0.5 0.35 1.0]})
                 (gt/triangle2 (cq/rel-pos 0.3 0.8)
                               (cq/rel-pos 0.4 0.9)
                               (cq/rel-pos 0.45 0.8))])})

(defn update-state [state]
  (update state :shapes (partial map (partial grow-clipped
                                              (:bounds state)
                                              (:shapes state)
                                              1.02))))

(defn draw [{:keys [shapes]}]
  (q/stroke-weight 0.8)
  (doseq [shape shapes
          :let [vertices (geom/vertices shape)]]
    (q/no-fill)
    (q/stroke 0)
    (when-let [color (:stroke (meta shape))]
      (apply q/stroke color))
    (cq/draw-shape vertices)
    (q/fill 0)
    (doseq [[x y] vertices]
      (q/ellipse x y 1.0 1.0))))

(defn ^:export run-sketch []
  ;; 20210502
  (q/defsketch polygrowth
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
