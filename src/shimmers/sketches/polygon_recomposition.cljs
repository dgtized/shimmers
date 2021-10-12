(ns shimmers.sketches.polygon-recomposition
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.probability :as p]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as geom]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.utils.delaunay :as delaunay]
   [thi.ng.geom.vector :as gv]))

(defn modify-points [points shape]
  (let [p (rand-nth points)
        p' (p/weighted {(geom/random-point-inside shape) 1
                        (p/confusion-disk p 1.0) 8
                        (p/confusion-disk p (rand)) 4})]
    (replace {p (gv/vec2 p')} points)))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [shape (rect/rect (cq/rel-vec 0.1 0.1) (cq/rel-vec 0.9 0.9))]
    {:shape shape
     :points (repeatedly 64 #(geom/random-point-inside shape))
     :hull nil
     :triangles []}))

(defn update-state [{:keys [points shape] :as state}]
  (let [new-points (modify-points points shape)
        polygon (gp/polygon2 (gp/convex-hull* new-points))]
    (assoc state
           :points new-points
           :hull polygon
           :triangles (delaunay/triangulate new-points))))

(defn draw [{:keys [points hull triangles]}]
  (q/background 1.0)
  (q/stroke 0.0 0.0 0.0)
  (q/stroke-weight 1.0)
  (q/no-fill)
  (doseq [p points]
    (cq/circle p 2.0))

  (q/stroke-weight 0.5)
  (when hull
    (q/begin-shape)
    (doseq [segment (geom/vertices hull)]
      (apply q/vertex segment))
    (q/end-shape :close))

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
