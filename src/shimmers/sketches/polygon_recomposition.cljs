(ns shimmers.sketches.polygon-recomposition
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.utils.delaunay :as delaunay]
   [thi.ng.math.core :as tm]))

(defn modify-points [points shape]
  (let [p (dr/rand-nth points)
        p' (rp/sample-point-inside
            (dr/weighted {shape 1
                          (gc/circle p 1.0) 8
                          (gc/circle p (dr/random)) 4}))]
    (replace {p p'} points)))

(defn setup []
  (q/color-mode :rgb 1.0)
  (let [shape (cq/screen-rect 0.8)]
    {:shape shape
     :points (repeatedly 64 #(g/random-point-inside shape))
     :hull nil
     :triangles []}))

(defn update-state [{:keys [points shape] :as state}]
  (let [new-points (modify-points points shape)
        polygon (gp/polygon2 (gp/convex-hull* new-points))]
    (assoc state
           :points new-points
           :hull polygon
           :triangles (delaunay/triangulate new-points))))

(defn draw [{:keys [shape triangles] :as state}]
  (q/background 1.0)
  (q/stroke 0.0)
  (q/stroke-weight 1.0)
  (q/no-fill)
  ;; (doseq [p (:points state)]
  ;;   (cq/circle p 2.0))

  (q/stroke-weight 0.5)
  (when-let [hull (:hull state)]
    (cq/draw-polygon hull))

  (q/no-stroke)
  (doseq [t triangles
          :let [time (* (q/frame-count) 0.005)
                center (g/centroid (gt/triangle2 t))
                noise (apply q/noise (conj (tm/* center 0.005) time))
                [u v] (g/map-point shape center)]]
    (q/fill 0.0
            (tm/clamp01 (- 1.0 v))
            (tm/clamp01 (+ u (eq/unit-sin (* 0.1 time))))
            (+ 0.45 (* noise 0.35)))
    (apply cq/draw-triangle t)))

(defn page []
  (sketch/component
   :size [800 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition polygon-recomposition
  {:created-at "2021-10-12"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
