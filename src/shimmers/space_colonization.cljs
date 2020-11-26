(ns ^:figwheel-hooks shimmers.space-colonization
  "Inspired by https://thecodingtrain.com/CodingChallenges/017-spacecolonizer.html and
  https://medium.com/@jason.webb/space-colonization-algorithm-in-javascript-6f683b743dc5
  Algorithm is from http://algorithmicbotany.org/papers/colonization.egwnp2007.html"
  (:require [goog.dom :as dom]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [reagent.core :as r]
            [reagent.dom :as rdom]
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
      (v/add (jitter -0.2 0.2))
      v/normalize
      (v/scale (/ 1 (count attractors)))
      v/normalize))

(comment
  (v/normalize (v/vec2 2 2))
  (v/sub (v/vec2 2 2) (v/vec2 0 0))
  (reduce v/add (map v/normalize [(v/vec2 2 2) (v/vec2 2 2)]))
  (v/scale (v/vec2 4 4) (/ 1 2))
  (average-attraction {:position (v/vec2 0 0)} [(v/vec2 2 2) (v/vec2 2 2)]))

(defn influencing-attractors [{:keys [attractors influence-distance branches]}]
  (into {}
        (for [attractor attractors
              :let [influences (influenced-branches attractor influence-distance branches)]
              :when (seq influences)]
          [attractor (closest-branch attractor influences)])))

(defn influence-direction [closest influencers]
  (->> influencers
       (keep (fn [[attractor branch]] (when (= branch closest) attractor)))
       (average-attraction closest)))

(defn grow [{:keys [influence-distance segment-distance prune-distance
                    branches attractors]
             :as state}]
  (let [influencers (influencing-attractors state)
        closest-branches (vals influencers)
        growth (for [closest closest-branches
                     :let [direction (influence-direction closest influencers)]]
                 (grow-branch closest direction segment-distance))
        prune (->> influencers
                   keys
                   (filter (fn [attractor]
                             (some (fn [branch] (< (branch-distance attractor branch)
                                                  prune-distance))
                                   growth)))
                   set)]
    (if (and (empty? growth) (empty? prune))
      (if (:completed-frame state)
        state
        (assoc state :completed-frame (q/frame-count)))
      (do
        (println {:growth (mapv :position growth)
                  :prune prune
                  :branches (count branches)})
        (assoc state
               :branches (concat branches growth)
               :attractors (remove prune attractors))))))

(defn init-settings []
  {:debug false})

(defonce settings (r/atom (init-settings)))

(defn setup []
  (q/frame-rate 10)
  {:influence-distance 24
   :prune-distance 4
   :segment-distance 4
   :attractors (repeatedly 256 #(v/vec2 (+ 40 (q/random (- (q/width) 80)))
                                        (+ 30 (q/random (- (q/height) 40)))))
   :branches [(make-branch nil (v/vec2 (/ (q/width) 2) (- (q/height) 5)))]})

(defn update-state [state]
  (let [fc (q/frame-count)
        diff (- fc (get state :completed-frame fc))]
    (if (> (/ diff (q/current-frame-rate)) 5)
      (setup)
      (grow state))))

(defn draw-attractor [[x y] influence prune]
  (q/stroke-weight 0.2)
  (q/stroke "lightblue")
  (q/ellipse x y influence influence)
  (q/stroke "red")
  (q/ellipse x y prune prune))

(defn draw [{:keys [attractors branches influence-distance prune-distance] :as state}]
  (q/ellipse-mode :radius)
  (q/background "white")
  (q/no-fill)
  (doseq [[x y] attractors]
    (q/stroke-weight 1)
    (q/stroke "green")
    (q/point x y))
  (q/stroke "black")
  (q/stroke-weight 0.5)
  (doseq [branch branches]
    (when-let [parent (:parent branch)]
      (q/line (:position parent) (:position branch))))

  (when (:debug @settings)
    (let [influencers (influencing-attractors state)]
      (doseq [[attractor branch] influencers
              :let [[x y] attractor]]
        (draw-attractor attractor influence-distance prune-distance)

        (q/stroke-weight 0.05)
        (q/stroke 128 128)
        (q/line (:position branch) attractor)

        (q/stroke-weight 0.2)
        (q/stroke 0 0 200 128)
        (q/line (:position branch)
                (v/add (:position branch)
                       (v/scale (influence-direction branch influencers) 5)))))))

(defn explanation []
  (let [{:keys [debug]} @settings]
    [:div
     [:input {:type "checkbox" :checked debug
              :on-click #(swap! settings update-in [:debug] not)}]
     [:label "Debug"]]))

(defn mount-reagent
  "Mounts reagent component to render in explanation element.

  Helper method so it can be invoked on run-sketch OR on figwheel reload."
  []
  (rdom/render [explanation] (dom/getElement "explanation")))

(defn ^:export run-sketch []
  (mount-reagent)
  (q/defsketch space-colonization
    :host "quil-host"
    :size [200 200]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode]))

;; reload reagent components after figwheel save
(defn ^:after-load after-reload []
  (mount-reagent))
