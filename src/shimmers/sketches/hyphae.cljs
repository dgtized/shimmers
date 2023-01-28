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
   :downstream 0})

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

(defn influenced-branches [{:keys [branches branches-tree attractors]} multiple]
  (apply merge-with set/union
         (for [{:keys [p r] :as attractor} attractors
               :let [neighbor (saq/nearest-neighbor-node branches-tree p)]
               :when (and neighbor (< (g/dist (:p (g/get-point-data neighbor)) p) (* multiple r)))]
           (let [branch (nth branches (:branch-idx (g/get-point-data neighbor)))]
             {branch #{attractor}}))))

(defn average-attraction [parent position attractors]
  (-> (reduce (fn [acc {:keys [p r]}]
                (let [dir (tm/- p position)]
                  (tm/+ acc (tm/normalize dir (/ r (tm/mag-squared dir))))))
              (tm/+ (dr/randvec2) (tm/normalize (tm/- position parent)))
              attractors)
      tm/normalize))

(defn grow-branches [{:keys [branches branches-tree]} influenced]
  (reduce (fn [[b bt buds]
              [{:keys [idx parent-idx position]} attractors]]
            (let [length (dr/random 1.0 5.0)
                  parent (or (and parent-idx (:position (nth branches parent-idx)))
                             position)
                  dir (average-attraction parent position attractors)
                  growth-pos (tm/+ position (tm/* dir length))
                  b' (add-branch b idx growth-pos)
                  new-branch (peek b')]
              (if-let [bt' (add-branch-tree bt new-branch)]
                [b' bt' (conj buds new-branch)]
                [b bt buds])))
          [branches branches-tree []]
          influenced))

(defn pruned [tree influenced attractors]
  (let [considered (apply set/union (vals influenced))
        pruning (filter (fn [{:keys [p r]}]
                          (when-let [neighbor (saq/nearest-neighbor-node tree p)]
                            (< (g/dist (:p (g/get-point-data neighbor)) p)
                               (max (* 0.01 r) 1.0))))
                        considered)]
    (remove (set pruning) attractors)))

(defn propagate-weight [branches idx]
  (if-let [{:keys [parent-idx]} (nth branches idx)]
    (if parent-idx
      (recur (update-in branches [parent-idx :downstream] inc)
             parent-idx)
      branches)
    branches))

(defn update-weights [branches buds]
  (reduce (fn [branches bud]
            (propagate-weight branches (:idx bud))) branches buds))

(defn grow [{:keys [attractors branches] :as state}]
  (if (empty? attractors)
    (assoc state :steady-state true)
    (let [[depth influenced]
          (some (fn [depth]
                  (when-let [s (seq (influenced-branches state (Math/pow 2 depth)))]
                    [depth s]))
                [0 2 4])]
      (if (empty? influenced)
        (assoc state :steady-state true)
        (let [[branches' tree' buds] (grow-branches state influenced)]
          (println {:depth depth
                    :influenced (count influenced)
                    :buds (count buds)
                    :branches (count branches)
                    :attractors (count attractors)})
          (assoc state
                 :attractors (pruned tree' influenced attractors)
                 :branches (update-weights branches' buds)
                 :branches-tree tree'))))))

(defn attractor-size []
  (cq/rel-h (dr/random 0.05 0.2)))

(defn attractors-circle [center]
  (fn [] (gc/circle (v/+polar center
                             (cq/rel-h (Math/sqrt (dr/random 0.08 0.2)))
                             (dr/random eq/TAU))
                   (attractor-size))))

(defn attractor-line [a b]
  (fn [] (gc/circle (tm/+ (tm/mix a b (dr/random)) (dr/randvec2 (dr/random (cq/rel-h 0.05))))
                   (attractor-size))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect)
        center (cq/rel-vec 0.5 0.5)
        attractors
        (concat (repeatedly 150 (attractors-circle center))
                (repeatedly 50 (attractor-line (cq/rel-vec 0.05 0.5) (cq/rel-vec 0.95 0.5)))
                (repeatedly 50 (attractor-line (cq/rel-vec 0.5 0.05) (cq/rel-vec 0.5 0.95))))
        branches (add-root [] (cq/rel-vec (dr/random 0.3 0.7) (dr/random 0.3 0.7)))]
    {:bounds bounds
     :attractors attractors
     :branches branches
     :branches-tree (reduce add-branch-tree (saq/circletree bounds) branches)}))

(defn update-state [state]
  (grow state))

(defn draw [{:keys [attractors branches]}]
  (q/background 1.0)
  (q/no-fill)
  (q/stroke 0.4 0.5 0.5 0.5)
  (q/stroke-weight 0.5)
  (doseq [a attractors]
    (cq/circle a))

  (q/stroke 0.0)
  (doseq [{:keys [parent-idx position downstream]} branches]
    (when-let [{parent :position} (and parent-idx (nth branches parent-idx nil))]
      (let [weight (min (max 0.2 (* 5.0 (/ downstream 1000))) 5.0)]
        (q/stroke-weight weight))
      (q/line parent position))))

(sketch/defquil hyphae
  :created-at "2023-01-24"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
