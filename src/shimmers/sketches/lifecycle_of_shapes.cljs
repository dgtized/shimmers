(ns shimmers.sketches.lifecycle-of-shapes
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(defn correspondences [triangles]
  (apply mapv
         vector
         (for [triset triangles
               :let [indicies (range (count triset))]]
           (dr/shuffle indicies))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [core-shapes (-> (cq/screen-rect 0.95)
                        (g/subdivide {:rows 4 :cols 4})
                        dr/shuffle)
        shapes (drop 4 core-shapes)
        triangles (map #(g/tessellate % {:cols 5 :rows 3}) (dr/shuffle shapes))]
    {:t 0.0
     :shapes shapes
     :triangles triangles
     :correspondences (correspondences triangles)}))

(defn update-state [state]
  (update state :t + 0.0005))

(defn draw [{:keys [t triangles correspondences] :as state}]
  (q/stroke-weight 0.3)
  ;; (doseq [s (:shapes state)]
  ;;   (cq/draw-shape (g/vertices s)))
  ;; (doseq [children triangles
  ;;         t children]
  ;;   (cq/draw-shape (g/vertices t)))
  (q/stroke-weight 0.8)
  (when (= 0 (mod (q/frame-count) 3))
    (doseq [correlate correspondences
            :let [triset (map (fn [i tessellation] (nth tessellation i)) correlate triangles)
                  n-states (count triset)
                  raw-offset (* (mod t 1.0) n-states)
                  o1 (int raw-offset)
                  [pts1 pts2] (map (comp :points (partial nth triset))
                                   [o1 (mod (inc o1) n-states)])
                  t-delta (- raw-offset o1)
                  vertices (map (fn [v1 v2] (tm/mix v1 v2 t-delta)) pts1 pts2)]]
      (apply cq/draw-triangle vertices))))

(sketch/defquil lifecycle-of-shapes
  :created-at "2021-12-28"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
