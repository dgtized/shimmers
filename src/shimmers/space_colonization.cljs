(ns shimmers.space-colonization
  "Inspired by https://thecodingtrain.com/CodingChallenges/017-spacecolonizer.html and
  https://medium.com/@jason.webb/space-colonization-algorithm-in-javascript-6f683b743dc5
  Algorithm is from http://algorithmicbotany.org/papers/colonization.egwnp2007.html"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.math.vector :as v]))

(defn make-branch [parent position]
  {:position position :parent parent})

(defn grow-branch [parent direction length]
  (make-branch parent
               (v/add (:position parent)
                      (v/scale direction length))))

(defn branch-distance [attractor branch]
  (v/distance attractor (:position branch)))

(defn influenced-branches [attractor influence-distance branches]
  (filter (fn [branch] (< (branch-distance attractor branch) influence-distance))
          branches))

(defn closest-branch [attractor branches]
  (apply min-key (partial branch-distance attractor) branches))

(defn jitter [lower upper]
  (let [r (- upper lower)]
    (v/vec2 (+ (rand r) lower) (+ (rand r) lower))))

(defn average-attraction [branch attractors]
  (-> (->> attractors
           (map #(v/normalize (v/sub % (:position branch))))
           (reduce v/add))
      (v/add (jitter -2 2))
      v/normalize
      (v/scale (/ 1 (count attractors)))
      v/normalize))

(defn influencing-attractors [{:keys [attractors influence-distance branches]}]
  (into {}
        (for [attractor attractors
              :let [influences (influenced-branches attractor influence-distance branches)]
              :when (seq influences)]
          [attractor influences])))

(defn influenced-by [branch influence-distance attractors]
  (filter (fn [attractor] (< (branch-distance attractor branch) influence-distance))
          attractors))

(defn grow [{:keys [influence-distance segment-distance prune-distance
                    branches attractors]
             :as state}]
  (let [influencers (influencing-attractors state)
        closest-branches (distinct (for [[attractor influences] influencers]
                                     (closest-branch attractor influences)))
        growth (for [closest closest-branches
                     :let [direction (->> (keys influencers)
                                          (influenced-by closest influence-distance)
                                          (average-attraction closest))]]
                 (grow-branch closest direction segment-distance))
        prune (->> influencers
                   keys
                   (filter (fn [attractor]
                             (some (fn [branch] (< (branch-distance attractor branch)
                                                  prune-distance))
                                   growth)))
                   set)]
    (if (and (empty? growth) (empty? prune))
      state
      (do
        (println {:growth (mapv :position growth)
                  :prune prune
                  :branches (count branches)})
        (assoc state
               :branches (concat branches growth)
               :attractors (remove prune attractors))))))

(defn setup []
  {:influence-distance 40
   :prune-distance 5
   :segment-distance 5
   :attractors (repeatedly 256 #(v/vec2 (+ 40 (q/random (- (q/width) 80)))
                                        (+ 30 (q/random (- (q/height) 40)))))
   :branches [(make-branch nil (v/vec2 200 400))]})

(defn update-state [state]
  (grow state)
  ;; state
  )

(defn draw-attractor [[x y] influence prune]
  (q/stroke-weight 1)
  (q/stroke "green")
  (q/point x y)
  (q/stroke-weight 0.2)
  (q/stroke "lightblue")
  (q/ellipse x y influence influence)
  (q/stroke "red")
  (q/ellipse x y prune prune))

(defn draw [{:keys [attractors branches influence-distance prune-distance]}]
  (q/ellipse-mode :radius)
  (q/background "white")
  (q/no-fill)
  (doseq [p attractors]
    (draw-attractor p influence-distance prune-distance))
  (q/stroke "black")
  (q/stroke-weight 0.5)
  (doseq [branch branches]
    (when-let [parent (:parent branch)]
      (q/line (:position parent) (:position branch)))))

(defn ^:export run-sketch []
  (q/defsketch space-colonization
    :host "quil-host"
    :size [400 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode]))


