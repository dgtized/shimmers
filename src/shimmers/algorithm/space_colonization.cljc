(ns shimmers.algorithm.space-colonization
  (:require [clojure.set :as set]
            [shimmers.math.vector :as v]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.spatialtree :as spatialtree]))

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
            (v/add (:position parent)
                   (v/scale direction length))
            direction))

(defn branch-distance [attractor branch]
  (v/distance attractor (:position branch)))

(defn influenced-branches [quadtree radius position]
  (spatialtree/select-with-circle quadtree position radius))

(defn close-to-branch? [quadtree radius position]
  (spatialtree/points-in-circle? quadtree position radius))

(defn closest-branch [attractor branches]
  (apply min-key (partial branch-distance attractor) branches))

(defn average-attraction
  [{:keys [position direction]} attractors]
  (-> (reduce (fn [acc attractor]
                (v/add acc (v/sub attractor position)))
              direction
              attractors)
      (v/add (v/jitter 1.0))
      (v/scale (/ 1 (+ (count attractors) 2)))
      v/normalize))

(comment
  (v/normalize (v/vec2 2 2))
  (v/sub (v/vec2 2 2) (v/vec2 0 0))
  (reduce v/add (map v/normalize [(v/vec2 2 2) (v/vec2 2 2)]))
  (v/scale (v/vec2 4 4) (/ 1 2))
  (average-attraction {:position (v/vec2 0 0) :direction (v/vec2 0 0)}
                      [(v/vec2 2 2) (v/vec2 2 2)])
  (average-attraction (->Branch nil (v/vec2 100 195) (v/vec2 0 -1))
                      [(v/vec2 112.0 189.0) (v/vec2 85.2 182.0) (v/vec2 [91.9 173.5])]))

(defn steady-state?
  "Check if growth is complete or has stalled somehow."
  [growth prune attractors]
  ;; (println {:growth (count growth) :prune (count prune) :attractors (count attractors)})
  (or
   ;; no remaining growth possible
   (empty? attractors)
   ;; no changes on this iteration
   (and (empty? growth) (empty? prune))))

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
            (geom/add-point tree (:position branch) branch))
          quadtree
          branches))

(defn influencing-attractors [attractors quadtree influence-distance]
  (apply merge-with set/union
         (for [attractor attractors
               :let [influences (influenced-branches quadtree influence-distance attractor)]
               :when (seq influences)]
           {(closest-branch attractor influences) #{attractor}})))

(defn grow-branches
  [branches influencers segment-distance snap-theta]
  (let [branch-index (->> branches
                          (map-indexed (fn [idx branch] {branch idx}))
                          (into {}))]
    (for [[branch attractors] influencers]
      (grow-branch branch (get branch-index branch)
                   (v/snap-to (average-attraction branch attractors) snap-theta)
                   segment-distance))))

(defn pruning-set
  [closest-fn influencers]
  (->> influencers
       vals
       (apply set/union)
       (filter closest-fn)
       set))

(defn grow
  [{:keys [influence-distance segment-distance prune-distance snap-theta
           attractors branches quadtree weights]
    :as state}]
  (let [influencers (influencing-attractors attractors quadtree influence-distance)
        growth
        (->> (grow-branches branches influencers segment-distance snap-theta)
             ;; remove any branches created too close to an existing branch this
             ;; forces it to select a given jitter influenced branch instead of
             ;; repeatedly adding new children at a point.
             (remove (fn [branch]
                       (close-to-branch? quadtree (/ segment-distance 4)
                                         (:position branch))))
             vec)

        new-quadtree (add-branch-positions quadtree growth)
        prune (pruning-set (partial close-to-branch? new-quadtree prune-distance)
                           influencers)
        new-branches (vec (concat branches growth))]
    (if (steady-state? growth prune attractors)
      (assoc state :steady-state true)
      (assoc state
             :weights (update-weights weights new-branches growth)
             :branches new-branches
             :attractors (remove prune attractors)
             :quadtree new-quadtree))))

(defn make-root [position direction]
  (->Branch nil position direction))

(defn create-tree
  [{:keys [bounds attractors branches
           influence-distance prune-distance segment-distance snap-theta]}]
  {:influence-distance influence-distance
   :prune-distance prune-distance
   :segment-distance segment-distance
   :snap-theta snap-theta
   :attractors attractors
   :branches branches
   :weights (update-weights {} branches branches)
   :quadtree (add-branch-positions (spatialtree/quadtree bounds) branches)})

