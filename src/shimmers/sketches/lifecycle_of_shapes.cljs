(ns shimmers.sketches.lifecycle-of-shapes
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.sequence :as cs]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]))

(defn correspondences [triangles]
  (apply mapv
         vector
         (for [triset triangles
               :let [indicies (range (count triset))]]
           (dr/shuffle indicies))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [shapes [(rect/rect (cq/rel-vec 0.1 0.1) (cq/rel-vec 0.3 0.3))
                (rect/rect (cq/rel-vec 0.2 0.4) (cq/rel-vec 0.4 0.8))
                (rect/rect (cq/rel-vec 0.6 0.6) (cq/rel-vec 0.9 0.9))]
        triangles (map #(g/tessellate % {:cols 5 :rows 3}) shapes)]
    {:shapes shapes
     :triangles triangles
     :correspondences (correspondences triangles)}))

(defn update-state [state]
  state)

(defn draw [{:keys [shapes triangles correspondences]}]
  (q/background 1.0)
  (q/stroke-weight 0.3)
  (doseq [s shapes]
    (cq/draw-shape (g/vertices s)))

  (doseq [children triangles
          t children]
    (cq/draw-shape (g/vertices t)))

  (q/stroke-weight 0.8)
  (doseq [correlate (take 2 correspondences)]
    (doseq [[t1 t2] (cs/pair-cycle (map (fn [i triset] (nth triset i)) correlate triangles))]
      (q/line (g/centroid t1) (g/centroid t2)))))

(sketch/defquil lifecycle-of-shapes
  :created-at "2021-12-28"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
