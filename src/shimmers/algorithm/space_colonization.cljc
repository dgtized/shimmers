(ns shimmers.algorithm.space-colonization
  (:require [clojure.set :as set]
            [shimmers.math.vector :as v]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.spatialtree :as spatialtree]
            [thi.ng.math.core :as tm]))

;; Ideas:
;;  * attractors could have influence PER attractor instead of global, or a weight on their influence?
;;  * some implementations have a branching likelyhood, or can just experiment with only creating one leaf per branch per cycle?
;;    - using distinct on closest branch kinda does this, but looks odd, maybe (take 2) of each unique branch or only N per iteration?
;;    - need kd-tree, or voronoi for faster "closest" lookups
;;  * build up an initial trunk if no attractors are in range?
;;  * add more weight to roots of the tree
;;  * improve rules for detecting steady state completion
;;  * grow to fit shapes or other distributions of attractors
(defrecord Branch [parent position direction])

(defn grow-branch [parent parent-idx direction length]
  (->Branch parent-idx
            (tm/+ (:position parent) (tm/normalize direction length))
            direction))

(defn branch-distance [attractor branch]
  (g/dist attractor (:position branch)))

(defn influenced-branches [quadtree radius position]
  (spatialtree/select-with-circle quadtree position radius))

(defn close-to-branch? [quadtree radius position]
  (spatialtree/points-in-circle? quadtree position radius))

(defn closest-branch [attractor branches]
  (apply min-key (partial branch-distance attractor) branches))

(defn average-attraction
  ([branch attractors]
   (average-attraction branch attractors 0 (v/vec2)))
  ([{:keys [position direction]} attractors snap-theta jitter]
   (-> (reduce (fn [acc attractor]
                 (tm/+ acc (tm/- attractor position)))
               direction attractors)
       (tm/+ jitter)
       tm/normalize
       (v/snap-to snap-theta))))

;; Approach borrowed from
;; https://github.com/jasonwebb/2d-space-colonization-experiments/blob/master/core/Network.js#L108-L114
(defn propagate-branch-weight [weights branches branch]
  (if-let [parent-idx (:parent branch)]
    (let [parent (nth branches parent-idx)
          current-weight (get weights branch)]
      (recur (update weights parent
                     (fn [parent-weight]
                       (if (< parent-weight (+ current-weight 0.1))
                         (+ parent-weight 0.01)
                         parent-weight)))
             branches
             parent))
    weights))

(defn update-weights [weights branches growth]
  (reduce-kv
   (fn [weights _ bud]
     (propagate-branch-weight (assoc weights bud 0.05) branches bud))
   weights growth))

(defn add-branch-positions [quadtree branches]
  ;; CAUTION: if add-point fails the return value is nil
  ;; I believe this happens if point is out of bounds of the quadtree
  (reduce (fn [tree branch]
            (g/add-point tree (:position branch) branch))
          quadtree
          branches))

(defn influencing-attractors [{:keys [quadtree influence-distance]} attractors]
  (apply merge-with set/union
         (for [attractor attractors
               :let [influences (influenced-branches quadtree influence-distance attractor)]
               :when (seq influences)]
           {(closest-branch attractor influences) #{attractor}})))

(defn grow-branches
  [segment-distance snap-theta jitter branches influencers]
  (let [branch-index (->> branches
                          (map-indexed (fn [idx branch] {branch idx}))
                          (into {}))]
    (for [[branch attractors] influencers]
      (grow-branch branch (get branch-index branch)
                   (average-attraction branch attractors snap-theta (jitter))
                   segment-distance))))

(defn grow-closest
  "Generate a branch growing towards the closest attractor.

  This is the fallback case if no attractor is close enough to influence a
  branch. It grows faster as a slight optimization as this case costs
  branches*attractors comparisons per call."
  [{:keys [segment-distance snap-theta]} branches attractors]
  (let [branch-index (->> branches
                          (map-indexed (fn [idx branch] {branch idx}))
                          (into {}))
        [branch attractor]
        (apply min-key (fn [[branch bud]] (branch-distance bud branch))
               (for [bud attractors
                     branch branches]
                 [branch bud]))]
    (grow-branch branch (get branch-index branch)
                 (average-attraction branch [attractor] snap-theta (v/vec2))
                 (max segment-distance
                      (/ (branch-distance attractor branch) 2)))))

(defn pruning-set
  [quadtree prune-distance influencers]
  (->> influencers
       vals
       (apply set/union)
       (filter (partial close-to-branch? quadtree prune-distance))
       set))

(defn grow
  [{:keys [segment-distance prune-distance snap-theta jitter
           attractors branches quadtree weights]
    :as state}]
  (if (empty? attractors)
    (assoc state :steady-state true)
    (let [influencers (influencing-attractors state attractors)]
      (if (empty? influencers)
        (let [new-branch (grow-closest state branches attractors)
              branches' (conj branches new-branch)]
          (assoc state
                 :weights (update-weights weights branches' [new-branch])
                 :branches branches'
                 :quadtree (add-branch-positions quadtree [new-branch])))
        (let [growth (vec (grow-branches segment-distance snap-theta jitter branches influencers))
              quadtree' (add-branch-positions quadtree growth)
              pruned (pruning-set quadtree' prune-distance influencers)
              branches' (vec (concat branches growth))]
          (assoc state
                 :weights (update-weights weights branches' growth)
                 :branches branches'
                 :attractors (remove pruned attractors)
                 :quadtree quadtree'))))))

(defn grow-tree [state]
  (some (fn [{:keys [steady-state] :as s}] (when steady-state s))
        (iterate grow state)))

(defn make-root [position direction]
  (->Branch nil position direction))

(defn create-tree
  [{:keys [bounds attractors branches
           influence-distance prune-distance segment-distance snap-theta]}]
  {:influence-distance influence-distance
   :prune-distance prune-distance
   :segment-distance segment-distance
   :snap-theta snap-theta
   :jitter (partial v/jitter 1.0)
   :attractors attractors
   :branches branches
   :weights (update-weights {} branches branches)
   :quadtree (add-branch-positions (spatialtree/quadtree bounds) branches)})
