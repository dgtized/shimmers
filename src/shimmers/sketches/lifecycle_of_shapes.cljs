(ns shimmers.sketches.lifecycle-of-shapes
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.math.core :as tm]))

(defn correspondences [triangles]
  (apply mapv
         vector
         (for [triset triangles
               :let [indicies (range (count triset))]]
           (dr/shuffle indicies))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [shapes [(rect/rect (cq/rel-vec 0.05 0.1) (cq/rel-vec 0.25 0.3))
                (rect/rect (cq/rel-vec 0.2 0.4) (cq/rel-vec 0.4 0.8))
                (rect/rect (cq/rel-vec 0.75 0.15) (cq/rel-vec 0.85 0.25))
                (rect/rect (cq/rel-vec 0.1 0.8) (cq/rel-vec 0.3 0.9))
                (rect/rect (cq/rel-vec 0.4 0.4) (cq/rel-vec 0.6 0.6))
                (rect/rect (cq/rel-vec 0.6 0.6) (cq/rel-vec 0.95 0.9))]
        triangles (map #(g/tessellate % {:cols 5 :rows 3}) (dr/shuffle shapes))]
    {:t 0.0
     :shapes shapes
     :triangles triangles
     :correspondences (correspondences triangles)}))

(defn update-state [state]
  (update state :t + 0.0025))

(defn draw [{:keys [t triangles correspondences] :as state}]
  (q/stroke-weight 0.3)
  ;; (doseq [s (:shapes state)]
  ;;   (cq/draw-shape (g/vertices s)))
  ;; (doseq [children triangles
  ;;         t children]
  ;;   (cq/draw-shape (g/vertices t)))

  (q/stroke-weight 0.8)
  (doseq [correlate correspondences
          :let [triset (map (fn [i tessellation] (nth tessellation i)) correlate triangles)
                n-states (count triset)
                raw-offset (* (mod t 1.0) n-states)
                o1 (int raw-offset)
                o2 (mod (inc o1) n-states)
                triangle1 (nth triset o1)
                triangle2 (nth triset o2)
                [pts1 pts2] (map :points [triangle1 triangle2])
                t-delta (- raw-offset o1)
                vertices (map (fn [v1 v2] (tm/mix v1 v2 t-delta)) pts1 pts2)]]
    (apply cq/draw-triangle vertices)))

(sketch/defquil lifecycle-of-shapes
  :created-at "2021-12-28"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
