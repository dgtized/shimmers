(ns shimmers.sketches.pulsing-grid
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.polygon :as gp]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.utils.subdiv :as gsd]
            [thi.ng.math.core :as tm]))

(defn rounding [polygon]
  (->> polygon
       geom/vertices
       (gsd/subdivide-closed (:chaikin gsd/schemes))
       gp/polygon2))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [r (rect/rect (cq/rel-w 0.05) (cq/rel-h 0.35)
                     (cq/rel-w 0.90) (cq/rel-h 0.30))
        cells (geom/subdivide r {:rows 6 :cols 24})]
    {:cells (for [cell cells]
              (assoc (rounding (geom/as-polygon (geom/scale-size cell 0.9)))
                     :color [0 1.0]
                     :pulse [(tm/random 0.1 0.9) (tm/random 0 10)]))
     :t 0}))

(defn update-state [{:keys [cells t] :as state}]
  (assoc state :cells
         (for [{[period phase] :pulse :as cell} cells
               :let [color (tm/map-interval (Math/cos (* period (+ t phase)))
                                            [-1 1] [0 1])]]
           (assoc cell :color [color 1.0]))
         :t (+ t 0.03)))

(defn draw [{:keys [cells]}]
  (q/background 1.0)
  (q/no-stroke)
  (doseq [{:keys [color] :as cell} cells]
    (apply q/fill color)
    (cq/draw-shape (geom/vertices cell))))

(sketch/defquil pulsing-grid
  :created-at "2021-08-21"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
