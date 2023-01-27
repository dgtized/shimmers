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
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

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

(defn add-branch-tree [tree {:keys [position idx]}]
  (let [neighbor (saq/nearest-neighbor-node tree position)]
    (when (or (not neighbor)
              (let [{:keys [p r] :as nearby} (g/get-point-data neighbor)]
                (or (not nearby)
                    (> (g/dist p position) r))))
      (saq/add-point tree position
                     (assoc (gc/circle position 0.5) :branch-idx idx)))))

(comment
  (add-branch (add-root [] (gv/vec2 0 0)) 0 (gv/vec2 1 0)))

(defn influenced-branches [branches branches-tree attractors]
  (apply merge-with set/union
         (for [{:keys [p r] :as attractor} attractors
               :let [neighbor (saq/nearest-neighbor-node branches-tree p)]
               :when (and neighbor (< (g/dist (:p (g/get-point-data neighbor)) p) (* 4 r)))]
           (let [branch (nth branches (:branch-idx (g/get-point-data neighbor)))]
             {branch #{attractor}}))))

(defn average-attraction [position attractors]
  (-> (reduce (fn [acc {:keys [p]}]
                (tm/+ acc (tm/normalize (tm/- p position))))
              (dr/randvec2 0.66) attractors)
      tm/normalize))

(defn grow-branches [{:keys [branches branches-tree]} influenced]
  (reduce (fn [[b bt] [{:keys [idx position]} attractors]]
            (let [length (dr/random 1.5 3.0)
                  growth-pos (tm/+ position (tm/* (average-attraction position attractors)
                                                  length))
                  b' (add-branch b idx growth-pos)
                  new-branch (peek b')]
              (if-let [bt' (add-branch-tree bt new-branch)]
                [b' bt']
                [b bt])))
          [branches branches-tree]
          influenced))

(defn pruned [tree influenced attractors]
  (let [considered (apply set/union (vals influenced))
        pruning (filter (fn [{:keys [p r]}]
                          (when-let [neighbor (saq/nearest-neighbor-node tree p)]
                            (< (g/dist (:p (g/get-point-data neighbor)) p) (* 0.33 r))))
                        considered)]
    (remove (set pruning) attractors)))

(defn grow [{:keys [attractors branches branches-tree] :as state}]
  (if (empty? attractors)
    (assoc state :steady-state true)
    (let [influenced (influenced-branches branches branches-tree attractors)]
      (if (empty? influenced)
        (assoc state :steady-state true)
        (let [[branches' tree'] (grow-branches state influenced)]
          (assoc state
                 :attractors (pruned tree' influenced attractors)
                 :branches branches'
                 :branches-tree tree'))))))

(defn attractors-circle [center]
  (fn [] (gc/circle (v/+polar center
                             (cq/rel-h (Math/sqrt (dr/random 0.125 0.2)))
                             (dr/random eq/TAU))
                   (cq/rel-h (dr/random 0.01 0.05)))))

(defn attractor-line [a b]
  (fn [] (gc/circle (tm/+ (tm/mix a b (dr/random)) (dr/randvec2 (dr/random (cq/rel-h 0.05))))
                   (cq/rel-h (dr/random 0.01 0.05)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect)
        center (cq/rel-vec 0.5 0.5)
        attractors (concat (repeatedly 150 (attractors-circle center))
                           (repeatedly 50 (attractor-line (cq/rel-vec 0.05 0.5) (cq/rel-vec 0.95 0.5)))
                           (repeatedly 50 (attractor-line (cq/rel-vec 0.5 0.05) (cq/rel-vec 0.5 0.95))))
        branches (add-root [] (tm/+ (:p (dr/rand-nth attractors)) (dr/randvec2 8)))]
    {:bounds bounds
     :attractors attractors
     :branches branches
     :branches-tree (reduce add-branch-tree (saq/circletree bounds) branches)}))

(defn update-state [state]
  (when-not (:steady-state state)
    (println {:branches (count (:branches state))
              :attractors (count (:attractors state))}))
  (grow state))

(defn draw [{:keys [attractors branches]}]
  (q/background 1.0)
  (q/no-fill)
  (q/stroke 0.4 0.5 0.5 0.5)
  (doseq [a attractors]
    (cq/circle a))

  (q/stroke 0.0)
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
