(ns shimmers.sketches.baubles
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.common.quil :as cq]
   [thi.ng.geom.core :as g]
   [shimmers.common.sequence :as cs]
   [thi.ng.geom.polygon :as gp]
   [shimmers.math.vector :as v]
   [shimmers.math.equations :as eq]))

(defn build-shape [t]
  (let [s (+ 3 (int (mod t 12)))]
    (gp/polygon2
     (vec (map (fn [vert]
                 (v/+polar (cq/rel-vec 0.5 0.5) (* 0.3 (q/height))
                           (- (* vert (/ eq/TAU s)) (* eq/TAU 0.25))))
               (range s))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0
   :shapes []})

(defn update-state [{:keys [t] :as state}]
  (-> state
      (assoc :shapes [(build-shape t)])
      (update :t + 0.05)))

(defn draw-points [vertices]
  (doseq [v vertices]
    (cq/circle v 2.5)))

(defn draw [{:keys [shapes]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  (doseq [s shapes]
    (let [verts1 (g/vertices (g/translate s (cq/rel-vec -0.25 0.0)))]
      (draw-points verts1)
      (cq/draw-curve-shape verts1))
    (let [verts2 (cs/rotate 1 (g/vertices (g/translate s (cq/rel-vec 0.25 0.0))))]
      (draw-points verts2)
      (cq/draw-curve-shape verts2))))

(sketch/defquil baubles
  :created-at "2023-01-18"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
