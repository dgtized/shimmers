(ns ^:figwheel-hooks shimmers.sketches.space-colonization
  "Inspired by https://thecodingtrain.com/CodingChallenges/017-spacecolonizer.html and
  https://medium.com/@jason.webb/space-colonization-algorithm-in-javascript-6f683b743dc5
  Algorithm is from http://algorithmicbotany.org/papers/colonization.egwnp2007.html"
  (:require [goog.dom :as dom]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [shimmers.math.vector :as v]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.spatialtree :as spatialtree]
            [shimmers.framerate :as framerate]))

(defn init-settings []
  {:influence-distance 48
   :prune-distance 6
   :segment-distance 4
   :attractor-power 9
   :debug {:attractors true
           :bubbles false
           :canalization true
           :influenced-by false
           :next-branch false}})

(defonce settings (r/atom (init-settings)))

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

(defn jitter [amount]
  (v/scale (apply v/vec2 (q/random-2d)) amount))

(defn average-attraction [branch attractors]
  (-> (->> attractors
           (map #(v/normalize (v/sub % (:position branch))))
           (reduce v/add (:direction branch)))
      (v/scale (/ 1 (+ (count attractors) 2)))
      (v/add (jitter 0.15))
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

(defn influencing-attractors [{:keys [attractors quadtree influence-distance] :as state}]
  (apply merge-with into
         (for [attractor attractors
               :let [influences (influenced-branches quadtree influence-distance attractor)]
               :when (seq influences)]
           {(closest-branch attractor influences) [attractor]})))

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
  (reduce (fn [tree branch]
            (geom/add-point tree (:position branch) branch))
          quadtree
          branches))

(defn grow [{:keys [influence-distance segment-distance prune-distance
                    attractors branches quadtree weights]
             :as state}]
  (let [influencers (influencing-attractors state)
        branch-index (->> branches
                          (map-indexed (fn [idx branch] {branch idx}))
                          (into {}))
        growth
        (->>
         (for [[branch attractors] influencers]
           (grow-branch branch (get branch-index branch)
                        (average-attraction branch attractors)
                        segment-distance))
         (remove (fn [branch] (close-to-branch? quadtree (/ segment-distance 4) (:position branch))))
         vec)

        new-quadtree (add-branch-positions quadtree growth)
        prune (->> influencers
                   vals
                   (apply concat)
                   distinct
                   (filter (partial close-to-branch? new-quadtree prune-distance))
                   set)
        new-branches (concat branches growth)]
    (if (steady-state? growth prune attractors)
      (if (:completed-frame state)
        state
        (assoc state :completed-frame (q/frame-count)))
      (assoc state
             :weights (update-weights weights new-branches growth)
             :branches new-branches
             :attractors (remove prune attractors)
             :quadtree new-quadtree))))

(defn create-tree
  [[width height]
   {:keys [influence-distance prune-distance segment-distance attractor-power]
    :as settings}]
  (let [attractor-count (Math/pow 2 attractor-power)
        branches [(->Branch nil (v/vec2 (/ width 2) (- height 5)) (v/vec2 0 -1))]]
    {:influence-distance influence-distance
     :prune-distance prune-distance
     :segment-distance segment-distance
     :attractor-count attractor-count
     :attractors (repeatedly attractor-count
                             #(v/vec2 (+ 110 (q/random (- width 220)))
                                      (+ 30 (q/random (- height 40)))))
     :branches branches
     :weights (update-weights {} branches branches)
     :quadtree (add-branch-positions (spatialtree/quadtree 0 0 width height)
                                     branches)}))

(defn setup []
  ;; (.clear js/console)
  (q/frame-rate 15)
  (create-tree [(q/width) (q/height)] @settings))

(defn update-state [state]
  (let [fc (q/frame-count)
        diff (- fc (get state :completed-frame fc))]
    (if (> (/ diff (q/current-frame-rate)) 5)
      (create-tree [(q/width) (q/height)] @settings)
      (grow state))))

(defn draw-attractor [[x y] influence prune]
  (q/stroke-weight 0.2)
  (q/stroke "lightblue")
  (q/ellipse x y influence influence)
  (q/stroke "red")
  (q/ellipse x y prune prune))

(defn draw-debug [{:keys [attractors influence-distance prune-distance] :as state} debug]
  (when (:attractors debug)
    (doseq [[x y] attractors]
      (q/stroke-weight 1)
      (q/stroke "green")
      (q/point x y)))

  (when ((some-fn :bubbles :influenced-by :next-branch) debug)
    (let [influencers (influencing-attractors state)]
      (doseq [[branch active-attractors] influencers]
        (doseq [attractor active-attractors]
          (when (:bubbles debug)
            (draw-attractor attractor influence-distance prune-distance))

          (when (:influenced-by debug)
            (q/stroke-weight 0.05)
            (q/stroke 128 128)
            (q/line (:position branch) attractor)))

        (when (:next-branch debug)
          (q/stroke-weight 0.2)
          (q/stroke 0 0 200 128)
          (q/line (:position branch)
                  (v/add (:position branch)
                         (v/scale (average-attraction branch active-attractors) 5))))))))

(defn draw [{:keys [branches weights debug] :as state}]
  (let [debug (:debug @settings)]
    (q/ellipse-mode :radius)
    (q/background "white")
    (q/no-fill)

    (q/stroke "black")

    (doseq [branch branches]
      (when-let [parent (:parent branch)]
        (q/stroke-weight (if (:canalization debug) (get weights branch) 0.2))
        (q/line (:position (nth branches parent)) (:position branch))))

    (draw-debug state debug)))

(defn checkbox [label field-ref]
  [:div.label-set
   [:input {:type "checkbox" :checked (get-in @settings field-ref)
            :on-change #(swap! settings update-in field-ref not)}]
   [:label label]])

(defn slider [label field-ref [lower upper]]
  (let [value (get-in @settings field-ref)]
    [:div.label-set
     [:label (label value)]
     [:input {:type "range" :value value :min lower :max upper
              :on-change (fn [e] (swap! settings assoc-in field-ref (int (.-target.value e))))}]]))

(defn explanation []
  [:div
   (slider (fn [v] (str "Attractor Count " (Math/pow 2 v)))
           [:attractor-power] [4 12])
   (slider (fn [v] (str "Influence Distance " v))
           [:influence-distance] [10 100])
   (slider (fn [v] (str "Prune Distance " v))
           [:prune-distance] [2 50])
   (slider (fn [v] (str "Segment Distance " v))
           [:segment-distance] [1 30])
   (checkbox "Show Canalization" [:debug :canalization])
   (checkbox "Show Attractors" [:debug :attractors])
   (checkbox "Show Influence/Prune Bubbles" [:debug :bubbles])
   (checkbox "Show Influence-By Lines" [:debug :influenced-by])
   (checkbox "Show Next Branch Direction" [:debug :next-branch])])

(defn mount-reagent
  "Mounts reagent component to render in explanation element.

  Helper method so it can be invoked on run-sketch OR on figwheel reload."
  []
  (rdom/render [explanation] (dom/getElement "explanation")))

(defn ^:export run-sketch []
  (mount-reagent)
  (q/defsketch space-colonization
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))

;; Temporarily disable so it doesn't load on saving other sketches
;; ;; reload reagent components after figwheel save
;; (defn ^:after-load after-reload []
;;   (mount-reagent))
