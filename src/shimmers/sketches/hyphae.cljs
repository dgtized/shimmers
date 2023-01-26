(ns shimmers.sketches.hyphae
  (:require
   [clojure.set :as set]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.quadtree :as saq]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]))

(defn make-branch [parent index pos]
  {:idx index
   :parent-idx parent
   :position pos
   :children []
   :weight 1})

(defn add-root [branches pos]
  (conj branches (make-branch nil (count branches) pos)))

(defn add-branch [branches parent-idx pos]
  (if-let [parent (nth branches parent-idx nil)]
    (let [idx (count branches)]
      (-> branches
          (assoc parent-idx (update parent :children (fnil conj []) idx))
          (conj (make-branch parent-idx idx pos))))
    branches))

(comment
  (add-branch (add-root [] (gv/vec2 0 0)) 0 (gv/vec2 1 0)))

(defn influenced-branches [branches-tree attractors]
  (apply merge-with set/union
         (for [{:keys [p r] :as attractor} attractors
               :let [neighbor (saq/nearest-neighbor-node branches-tree p)]
               :when (and neighbor (< (:distance neighbor) r))]
           {(g/get-point-data (:node neighbor)) #{attractor}})))

#_(defn grow [{:keys [attractors branches-tree]} :as state]
    (if (empty? attractors)
      (assoc state :steady-state true)
      (let [influenced (influenced-branches branches-tree attractors)]
        (if (empty? influenced)
          (assoc state :steady-state true)
          state))))

(defn attractors-builder [center]
  (fn [] (gc/circle (v/+polar center
                             (cq/rel-h (Math/sqrt (dr/random 0.125 0.2)))
                             (dr/random eq/TAU))
                   (cq/rel-h (dr/random 0.01 0.05)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect)
        center (cq/rel-vec 0.5 0.5)
        attractors (repeatedly 128 (attractors-builder center))
        branches (add-branch (add-root [] center) 0 (cq/rel-vec 0.4 0.5))]
    {:bounds bounds
     :attractors attractors
     :branches branches
     :branches-tree (reduce (fn [t {:keys [idx position]}]
                              (saq/add-point t position idx))
                            (saq/circletree bounds) branches)}))

(defn update-state [state]
  state)

(defn draw [{:keys [attractors branches]}]
  (q/background 1.0)
  (q/no-fill)
  (doseq [a attractors]
    (cq/circle a))

  (doseq [{:keys [parent-idx position weight]} branches]
    (when-let [{parent :position} (and parent-idx (nth branches parent-idx nil))]
      (q/stroke-weight weight)
      (q/line parent position))))

(sketch/defquil hyphae
  :created-at "2023-01-24"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
