(ns shimmers.probabilistic-automata
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.framerate :as framerate]))

(defn in-bounds? [[x y]]
  (and (>= x 0) (< x (q/width))
       (>= y 0) (< y (q/height))))

(defn interpret [{:keys [position heading velocity] :as bot} instruction]
  (let [[op arg] instruction]
    (case op
      :rotate (assoc bot :heading (+ heading arg))
      :forward (let [[x y] position
                     velocity arg
                     new-position [(+ x (* velocity (q/cos heading)))
                                   (+ y (* velocity (q/sin heading)))]]
                 (if (in-bounds? new-position)
                   (assoc bot
                          :position new-position
                          :last-position position)
                   (assoc bot :state :halt))))))

(defn execute [{:keys [ip state program] :as bot}]
  (when (= state :running)
    (assoc (interpret bot (nth program ip))
           :ip (mod (inc ip) (count program)))))

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
   :ip 0
   :program program})

(def petals (compile [:forward :forward :left :forward :left [:rotate 1]]))

(defn setup
  []
  (q/background "white")
  {:automata [(make-automata petals)]})

(defn update-state
  [state]
  (update state :automata (partial map execute)))

(defn draw
  [{:keys [automata]}]
  (doseq [bot (filter #(= (:state %) :running) automata)
          :let [{:keys [position last-position]} bot]]
    (if last-position
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

