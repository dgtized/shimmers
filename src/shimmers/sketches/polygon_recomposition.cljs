(ns shimmers.sketches.polygon-recomposition
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as geom]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [shape (rect/rect (cq/rel-vec 0.1 0.1) (cq/rel-vec 0.9 0.9))
        points (repeatedly 64 #(geom/random-point-inside shape))
        polygon (gp/polygon2 (gp/convex-hull* points))]
    {:points points
     :hull polygon
     :triangles (gp/tessellate* polygon)}))

(defn update-state [state]
  state)

(defn draw [{:keys [points hull triangles]}]
  (q/background 1.0)
  (q/stroke 0.0 0.0 0.0)
  (q/stroke-weight 1.0)
  (q/no-fill)
  (doseq [p points]
    (cq/circle p 2.0))

  (q/stroke-weight 0.5)
  (q/begin-shape)
  (doseq [segment (geom/vertices hull)]
    (apply q/vertex segment))
  (q/end-shape :close)

  (q/stroke 0.0 0.5 0.5)
  (doseq [t triangles]
    (apply cq/draw-triangle t)))

(sketch/defquil polygon-recomposition
  :created-at "2021-10-12"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
