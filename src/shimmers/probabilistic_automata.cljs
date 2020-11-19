(ns shimmers.probabilistic-automata
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.framerate :as framerate]
            [shimmers.math.color :as color]
            [goog.dom :as dom]
            [goog.string.format]
            [reagent.core :as r]
            [reagent.dom :as rdom]))

(defn in-bounds? [[x y] bounds]
  (and (>= x (- bounds)) (< x (+ (q/width) bounds))
       (>= y (- bounds)) (< y (+ (q/height) bounds))))

(defn interpret-argument [arg]
  (if (vector? arg)
    (let [[op value] arg]
      (case op
        :random (rand-int value)
        :color (color/random-gradient value)
        arg))
    arg))

(defn interpret [{:keys [position heading velocity ip program] :as bot} instruction]
  (let [[op argument] instruction
        arg (interpret-argument argument)]
    (case op
      :heading (assoc bot :heading arg)
      :rotate (assoc bot :heading (+ heading arg))
      :forward (let [[x y] position
                     velocity arg
                     new-position [(+ x (* velocity (q/cos heading)))
                                   (+ y (* velocity (q/sin heading)))]]
                 (if (in-bounds? new-position 100)
                   (assoc bot :position new-position)
                   (interpret bot [:halt 0])))
      :color (assoc bot :color arg)

      :goto (assoc bot :ip (dec (+ ip (- (count program) (mod ip (count program))) arg)))
      :one-of (interpret bot (rand-nth arg))
      :halt (assoc bot :state :halt)
      :fork (assoc bot :state :forking))))

;; 0 1 2 3 4 5 ip
;; 0 1 2 0 1 2 (mod ip)
;; [[:forward 5] [:rotate 1] [:goto 0]]

;; 55
;; 1 (mod n program)
;; 0 (mod 0 program)

(def lifespan 1000)
(def max-population 128)

(defn execute [{:keys [ip state program] :as bot}]
  (cond (> ip lifespan) (assoc bot :state :halt)
        (= state :running)
        (update (assoc (interpret bot (nth program (mod ip (count program))))
                       :last-position (:position bot))
                :ip inc)
        (= state :forking)
        (assoc bot :state :running)
        :else bot))

(defn execute-all
  [automata]
  (let [alive (remove #(= (:state %) :halt) automata)
        forks (filter #(= (:state %) :forking) alive)]
    (map execute (concat (take-last max-population alive) forks))))

(defn op->instruction [op]
  (if (vector? op)
    op
    (case op
      :forward [:forward 50]
      :left [:rotate (- (/ Math/PI 2))]
      :right [:rotate (+ (/ Math/PI 2))])))

(defn compile [program]
  (map op->instruction program))

;; (weighted [frequency instruction] ...)
(defn weighted [& options]
  (into [] (mapcat (fn [[n instruction]]
                     (repeat n instruction))
                   (partition 2 options))))
(comment (weighted 1 [:forward 10] 3 [:forward 5]))

(defn rotate [degrees]
  [:rotate (/ (* 180 degrees) Math/PI)])

(defn make-automata [position program]
  {:position position
   :heading (* 3 (/ Math/PI 2))
   :last-position nil
   :state :running
   :color [0 0 0 10]
   :ip 0
   :program program})

(def petals (compile [:forward :forward :left :forward :left [:rotate 1]]))
(def skribbles [[:forward 20]
                [:one-of [[:forward 10] [:forward 20]]]
                [:rotate 1]
                [:one-of [[:color [0 50 200 50]] [:color [0 0 0 25]]]]])
(def test-halt [[:forward 50] [:halt 0]])
(def test-fork [[:forward 10]
                [:fork 0]
                [:one-of [(rotate 60) (rotate -60) [:halt 0]]]])

(def make-tree [[:forward 10]
                [:one-of [[:fork 0] [:fork 0] [:fork 0]
                          [:halt 0] [:halt 0] [:halt 0] [:halt 0]
                          [:rotate 0.5] [:rotate -0.5] [:rotate 0.25] [:rotate -0.25]
                          [:forward 2] [:forward 5] [:forward 2] [:forward 5]
                          [:heading (* 3 (/ Math/PI 2))]]]])

(def test-recursive [[:one-of [[:forward 10]
                               [:one-of [[:rotate 0.5] [:rotate -0.5]]]]]])

(def recursive-tree [[:forward 10]
                     [:one-of [[:one-of [[:rotate 0.5] [:rotate -0.5]]]
                               [:one-of [[:forward 2] [:forward 5]]]]]
                     [:one-of (weighted 2 [:forward 10]
                                        2 [:fork 0]
                                        1 [:heading (* 3 (/ Math/PI 2))]
                                        1 [:halt 0])]])

(def test-random [[:forward [:random 50]] [:rotate 1]])
(def test-goto [[:forward 100] [:rotate 1] [:forward 20] [:goto 1]])

(def test-interesting [[:rotate 4.2]
                       [:rotate 0.8]
                       [:one-of [[:forward 18.0]
                                 [:halt 0.0]
                                 [:rotate -0.8]]]
                       [:fork 0.0]
                       [:rotate -0.1]
                       [:forward 23.0]
                       [:heading 4.8]
                       [:rotate 0.3]])

(def test-interesting2 [[:forward 42.0]
                        [:fork 0.0]
                        [:color [0 0 0 10]]
                        [:heading 2.4]
                        [:one-of [[:forward 43.0]
                                  [:halt 0.0]
                                  [:fork 0.0]
                                  [:rotate 4.2]]]
                        [:fork 0.0]
                        [:rotate -0.9]
                        [:color [:color :rainbow1]]
                        [:color [:color :rainbow1]]
                        [:rotate 2.1]])

(defn generate-instruction []
  ((rand-nth
    (weighted 4 (fn [] [:forward (+ 1 (rand-int 50))])
              2 (fn [] [:rotate (rand (* Math/PI 2))])
              1 (fn [] (rotate 60))
              3 (fn [] [:rotate (- (rand (/ Math/PI 3)) (/ Math/PI 3))]) ;; small angles
              1 (fn [] [:heading (rand (* Math/PI 2))])
              2 (fn [] [:fork 0])
              1 (fn [] [:halt 0])
              3 (fn [] [:color [:color :rainbow1]])
              2 (fn [] [:color [0 0 0 10]])
              1 (fn [] [:color [255 255 255 255]])
              1 (fn [] [:one-of (repeatedly (+ 1 (rand-int 5)) generate-instruction)])))))

(defn generate-program [n]
  (repeatedly n generate-instruction))

(defn make-random-automata [position]
  (let [program (generate-program (+ 2 (rand-int 10)))]
    (print {:position position :program program})
    {:position position
     :heading (* 3 (/ Math/PI 2))
     :last-position nil
     :state :running
     :color [0 0 0 10]
     :ip 0
     :program program}))

(defn prettify-instruction [instruction]
  (let [[op argument] instruction
        arg (if (vector? argument)
              (print-str argument)
              (goog.string/format "%.1f" argument))]
    (case op
      :one-of (print-str [:one-of (->> argument
                                       (map prettify-instruction)
                                       (interpose "\n\t")
                                       vec)])
      (print-str [op arg]))))

(defn describe [bot]
  [:p
   (print-str (:position bot))
   [:pre {:style {:font-size 10}} (interpose [:br] (map prettify-instruction (:program bot)))]])

(defn render-explanation [automata]
  (let [explanation (dom/getElement "explanation")]
    (rdom/render [:div {:style {:display :grid :grid-template-columns "auto auto"}}
                  (map describe automata)] explanation)))

(defn setup
  []
  (q/background "white")
  (let [automata [(make-random-automata [150 100])
                  (make-random-automata [450 100])
                  (make-random-automata [150 300])
                  (make-random-automata [450 300])]]
    (render-explanation automata)
    {:automata automata}))

(defn update-state
  [state]
  (if (= (mod (inc (q/frame-count)) 1200) 0)
    (setup)
    (update state :automata execute-all)))

(defn draw
  [{:keys [automata]}]
  (doseq [bot (filter #(= (:state %) :running) automata)
          :let [{:keys [position last-position color]} bot]]
    (when (and last-position (not= position last-position))
      (apply q/stroke color)
      (q/line last-position position)))
  (framerate/display (q/current-frame-rate)))

(defn ^:export run-sketch []
  (q/defsketch particles
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode]))
