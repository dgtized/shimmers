(ns shimmers.sketches.polygrowth
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.color :as color]
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

(defn as-polygon [[shape color]]
  (with-meta (geom/as-polygon shape) {:stroke color}))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:bounds [(geom/scale-size (rect/rect 0 0 (q/width) (q/height)) 4)]
   :shapes (map as-polygon
                {(gc/circle (cq/rel-pos 0.4 0.5) 40) (color/hex->hsla "#3b4d61")
                 (gc/circle (cq/rel-pos 0.2 0.3) 30) (color/hex->hsla "#ef9d10")
                 (gt/triangle2 (cq/rel-pos 0.3 0.8)
                               (cq/rel-pos 0.4 0.9)
                               (cq/rel-pos 0.45 0.8))
                 (color/hex->hsla "#6b7b8c")})})

(defn update-state [state]
  (update state :shapes (partial map (partial grow-clipped
                                              (:bounds state)
                                              (:shapes state)
                                              1.02))))

(defn draw [{:keys [shapes]}]
  (q/ellipse-mode :radius)
  (q/stroke-weight 0.75)
  (doseq [shape shapes
          :let [vertices (geom/vertices shape)]]
    (q/no-fill)
    (q/stroke 0)
    (cq/color-if q/stroke (:stroke (meta shape)))
    (cq/draw-shape vertices)
    (q/fill 0)
    (doseq [v vertices]
      (cq/circle v 0.5))))

(defn ^:export run-sketch []
  ;; 20210502
  (q/defsketch polygrowth
    :host "quil-host"
    :size [900 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
