(ns shimmers.sketches.delaunay-voronoi
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.utils.delaunay :as delaunay]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn generate-points [n dist]
  (repeatedly n #(gv/vec2 (dist) (dist))))

(defn bisect
  [[p q]]
  (let [s (tm/- q p)]
    (->> [(/ Math/PI 3)
          (* 5 (/ Math/PI 3))]
         (mapv (fn [t] (geom/translate p (geom/rotate s t)))))))

(defn setup []
  (q/no-loop)
  (let [points (generate-points 4 #(q/random 0.15 0.85))]
    {:points points
     :triangles (delaunay/triangulate points)}))

(defn update-state [state]
  state)

(defn draw [{:keys [points triangles]}]
  (q/background 255)

  (q/stroke-weight 0.5)
  (q/no-fill)
  (doseq [triangle triangles
          :let [view-pts (map cq/rel-pos triangle)]]
    (apply q/triangle (flatten view-pts))
    (doseq [edge (geom/edges (apply gt/triangle2 view-pts))
            :let [[a b] (bisect edge)]]
      (q/line a b)))

  (q/ellipse-mode :radius)
  (q/fill 0 0 0)
  (doseq [p points
          :let [[x y] (cq/rel-pos p)]]
    (q/ellipse x y 1 1))
  )

(defn ^:export run-sketch []
  (q/defsketch delaunay-voronoi
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
