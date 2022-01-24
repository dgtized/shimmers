(ns shimmers.sketches.quadtree
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.debug :as debug]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.spatialtree :as spatialtree]
   [thi.ng.geom.vector :as gv]))

(defonce defo (debug/state))

(defn build-tree [{:keys [points] :as state}]
  (assoc state :tree
         (reduce (fn [t {:keys [p] :as c}] (g/add-point t p c))
                 (spatialtree/quadtree 0 0 (q/width) (q/height))
                 points)))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect 0.99)]
    (build-tree {:bounds bounds
                 :points (repeatedly 1000 #(gc/circle (g/random-point-inside bounds) 3.0))
                 :tree (spatialtree/quadtree bounds)
                 :mouse (gv/vec2)})))

(defn update-state [state]
  (let [mp (cq/mouse-position)]
    (assoc state :mouse mp)))

(defn draw-complete-tree [{:keys [tree]}]
  (let [traversal (tree-seq (fn [t] (not-empty (spatialtree/get-children t)))
                            (fn [t] (remove nil? (spatialtree/get-children t)))
                            tree)]
    (doseq [n traversal]
      (q/stroke 0.5)
      (cq/rectangle (g/bounds n))
      (when-let [circle (g/get-point-data n)]
        (q/stroke 0.0)
        (cq/rectangle (g/bounds n))
        (q/stroke 0.0 0.5 0.5)
        (cq/circle circle)))))

(defn draw [{:keys [bounds points mouse] :as state}]
  (q/background 1.0)
  (q/stroke-weight 0.66)
  (q/no-fill)
  (swap! defo assoc :mouse mouse)
  (if (g/contains-point? bounds mouse)
    (do
      (q/stroke 0.0 0.5 0.5)
      (doseq [circle points]
        (cq/circle circle)))
    (draw-complete-tree state)))

(sketch/defquil quadtree
  :created-at "2021-10-10"
  :tags #{:datastructures}
  :size [800 600]
  :on-mount #(debug/mount defo)
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
