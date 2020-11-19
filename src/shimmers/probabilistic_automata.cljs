(ns shimmers.probabilistic-automata
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.framerate :as framerate]))

(defn in-bounds? [[x y] bounds]
  (and (>= x (- bounds)) (< x (+ (q/width) bounds))
       (>= y (- bounds)) (< y (+ (q/height) bounds))))

(defn interpret [{:keys [position heading velocity] :as bot} instruction]
  (let [[op arg] instruction]
    (case op
      :rotate (assoc bot :heading (+ heading arg))
      :forward (let [[x y] position
                     velocity arg
                     new-position [(+ x (* velocity (q/cos heading)))
                                   (+ y (* velocity (q/sin heading)))]]
                 (if (in-bounds? new-position 100)
                   (assoc bot :position new-position)
                   (interpret bot [:halt 0])))
      :color (assoc bot :color arg)
      :one-of (interpret bot (rand-nth arg))
      :halt (assoc bot :state :halt))))

(defn execute [{:keys [ip state program] :as bot}]
  (if (= state :running)
    (assoc (interpret bot (nth program (mod ip (count program))))
           :last-position (:position bot)
           :ip (inc ip))
    bot))

(defn execute-all
  [automata]
  (map execute automata))

(defn op->instruction [op]
  (if (vector? op)
    op
    (case op
      :forward [:forward 50]
      :left [:rotate (- (/ Math/PI 2))]
      :right [:rotate (+ (/ Math/PI 2))])))

(defn compile [program]
  (map op->instruction program))

(defn make-automata [program]
  {:position [200 200]
   :heading 0
   :last-position nil
   :state :running
   :color [0 0 0 25]
   :ip 0
   :program program})

(def petals (compile [:forward :forward :left :forward :left [:rotate 1]]))
(def skribbles [[:forward 20] [:one-of [[:forward 10] [:forward 20]]] [:rotate 1] [:one-of [[:color [0 50 200 50]] [:color [0 0 0 25]]]]])
(def test-halt [[:forward 50] [:halt 0]])

(defn setup
  []
  (q/background "white")
  {:automata [(make-automata skribbles)
              (make-automata petals)]})

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

