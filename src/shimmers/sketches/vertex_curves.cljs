(ns shimmers.sketches.vertex-curves
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
   [shimmers.math.equations :as eq]
   [shimmers.common.ui.controls :as ctrl]))

(defn build-shape [t]
  (let [s (+ 3 (int (mod t 13)))]
    (gp/polygon2
     (vec (map (fn [vert]
                 (v/+polar (cq/rel-vec 0.5 0.5) (* 0.2 (q/height))
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
    (let [verts1 (g/vertices (g/translate s (cq/rel-vec -0.25 -0.25)))]
      (draw-points verts1)
      (cq/draw-curve-shape verts1)
      (cq/draw-polygon (g/translate s (cq/rel-vec -0.25 0.25))))
    (let [verts2 (cs/rotate 1 (g/vertices (g/translate s (cq/rel-vec 0.25 -0.25))))]
      (draw-points verts2)
      (cq/draw-curve-shape verts2)
      (let [v2' (mapv #(g/translate % (cq/rel-vec 0.0 0.5)) verts2)
            v-overlap (into v2' (take 2 v2'))]
        (draw-points v-overlap)
        (cq/draw-curve-shape v-overlap)))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:div.contained.explanation
    [:p.readable-width
     "Experimenting with curve vertex on polygons. The issue is that by default
   curve-vertex will use every 2nd vertex as a control point, but it leaves a
   straight line at the start."]]])

(sketch/definition vertex-curves
  {:created-at "2023-01-18"
   :tags #{:debug}
   :type :quil}
  (ctrl/mount page))
