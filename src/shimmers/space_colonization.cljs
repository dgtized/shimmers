(ns shimmers.space-colonization
  "Inspired by https://thecodingtrain.com/CodingChallenges/017-spacecolonizer.html and
  https://medium.com/@jason.webb/space-colonization-algorithm-in-javascript-6f683b743dc5
  Algorithm is from http://algorithmicbotany.org/papers/colonization.egwnp2007.html"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.math.vector :as v]
            [shimmers.framerate :as framerate]))

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

(defn average-attraction [branch attractors]
  (-> (->> attractors
           (map #(v/normalize (v/sub % (:position branch))))
           (reduce v/add))
      (v/scale (/ 1 (count attractors)))
      v/normalize))

(defn influencing-attractors [state]
  (into {}
        (for [attractor (:attractors state)
              :let [influences (influenced-branches attractor (:influence-distance state) (:branches state))]
              :when (seq influences)]
          [attractor influences])))

(defn influenced-by [branch influence-distance attractors]
  (filter (fn [attractor] (< (branch-distance attractor branch) influence-distance))
          attractors))

(defn grow [state]
  (let [influencers (influencing-attractors state)
        growth (for [[attractor influences] influencers
                     :let [closest (closest-branch attractor influences)
                           direction (->> (keys influencers)
                                          (influenced-by closest (:influence-distance state))
                                          (average-attraction closest))]]
                 (grow-branch closest direction (:segment-distance state)))
        prune (->> influencers
                   keys
                   (filter (fn [attractor]
                             (some (fn [branch] (< (branch-distance attractor branch)
                                                  (:prune-distance state)))
                                   growth)))
                   set)]
    (if (or (seq growth) (seq prune))
      (do
        (println {:growth (mapv :position growth) :prune prune})
        (assoc state
               :branches (concat (:branches state) growth)
               :attractors (remove prune (:attractors state))))
      state)))

(defn setup []
  {:influence-distance 120
   :prune-distance 40
   :segment-distance 5
   :attractors (repeatedly 128 #(v/vec2 (+ 10 (q/random (- (q/width) 20)))
                                        (+ 10 (q/random (- (q/height) 50)))))
   :branches [(make-branch nil (v/vec2 200 400))]})

(defn update-state [state]
  (grow state)
  ;; state
  )

(defn draw [{:keys [attractors branches]}]
  (q/background "black")
  (q/stroke "white")
  (doseq [p attractors]
    (apply q/point p))
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


