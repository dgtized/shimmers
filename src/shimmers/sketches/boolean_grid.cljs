(ns shimmers.sketches.boolean-grid
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.quadtree :as saq]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [circles (mapv (fn [p] (let [r (/ (tm/clamp (dr/gaussian 1.0 0.4) 0.1 2.0) 14.0)]
                               (gc/circle p (* 0.5 r (q/height)))))
                      (rp/poisson-disc-sampling (cq/screen-rect 0.875) 150))]
    {:circles circles
     :tree (saq/add-to-circletree (saq/circletree (cq/screen-rect)) circles)}))

(defn update-state [state]
  state)

(defn draw [{:keys [circles tree]}]
  (q/ellipse-mode :radius)
  (q/background 1.0)
  (q/no-fill)
  (q/stroke 0.0 1.0)
  (doseq [circle circles]
    (qdg/draw circle))

  (q/stroke 0.0 0.2)
  (doseq [{:keys [r] :as node} (saq/simple-traversal-tree tree)
          :let [circle (:d node)]]
    (let [neighbors (saq/k-nearest-neighbors tree 4 (:p circle))]
      (comment (qdg/draw (apply rect/rect r)))
      (doseq [overlap (keep (fn [neighbor]
                              (when (collide/overlaps? circle
                                                       (g/get-point-data neighbor))
                                (g/get-point-data neighbor)))
                            neighbors)]
        (qdg/draw (gl/line2 (:p circle) (:p overlap))))
      (qdg/draw (g/scale-size circle 0.9)))))

(defn page []
  [:div
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   [:div.centered.readable-width
    [:p "Genuary 2026 - Day 7 - Boolean Logic"]]])

(sketch/definition boolean-grid
  {:created-at "2026-01-07"
   :tags #{:genuary2026}
   :type :quil}
  (ctrl/mount page))
