(ns shimmers.sketches.quadtree
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.spatialtree :as spatialtree]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:points (repeatedly 1000 #(gc/circle (cq/rel-vec (rand) (rand)) 3.0))
   :tree (spatialtree/quadtree 0 0 (q/width) (q/height))})

(defn update-state [{:keys [points] :as state}]
  (assoc state :tree (reduce (fn [t {:keys [p] :as c}] (g/add-point t p c))
                             (spatialtree/quadtree 0 0 (q/width) (q/height))
                             points)))

(defn draw [{:keys [tree]}]
  (q/background 1.0)
  (q/stroke-weight 0.66)
  (q/no-fill)
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

(sketch/defquil quadtree
  :created-at "2021-10-10"
  :tags #{:datastructures}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
