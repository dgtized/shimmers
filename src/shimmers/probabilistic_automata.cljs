(ns shimmers.probabilistic-automata
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.framerate :as framerate]))

(defn in-bounds? [[x y] bounds]
  (and (>= x (- bounds)) (< x (+ (q/width) bounds))
       (>= y (- bounds)) (< y (+ (q/height) bounds))))

(defn interpret-argument [arg]
  (if (vector? arg)
    (let [[op value] arg]
      (case op
        :random (rand-int value)))
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

(defn make-automata [program]
  {:position [200 375]
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

(defn generate-instruction []
  (rand-nth (weighted 3 [:forward (rand-int 20)] 3 [:rotate (rand (* Math/PI 2))] 2 [:fork 0] 0 [:halt 0])))

(defn generate-program [n]
  (repeatedly n generate-instruction))

(defn setup
  []
  (q/background "white")
  (let [program (generate-program (+ 2 (rand-int 5)))]
    (print program)
    {:automata [(make-automata program)]})) 

(defn update-state
  [state]
  (update state :automata execute-all))

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
    :size [400 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode]))

