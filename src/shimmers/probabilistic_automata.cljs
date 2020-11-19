(ns shimmers.probabilistic-automata
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.framerate :as framerate]))

(defn in-bounds? [[x y]]
  (and (>= x 0) (< x (q/width))
       (>= y 0) (< y (q/height))))

(defn interpret [{:keys [position heading velocity] :as bot} instruction]
  (case instruction
    :left (assoc bot :heading (- heading (/ Math/PI 2)))
    :right (assoc bot :heading (+ heading (/ Math/PI 2)))
    :forward (let [[x y] position
                   new-position [(+ x (* velocity (q/cos heading)))
                                 (+ y (* velocity (q/sin heading)))]]
               (if (in-bounds? new-position)
                 (assoc bot
                        :position new-position
                        :last-position position)
                 (assoc bot :state :halt)))))

(defn execute [{:keys [ip state program] :as bot}]
  (when (= state :running)
    (assoc (interpret bot (nth program ip))
           :ip (mod (inc ip) (count program)))))

(defn make-automata []
  {:position [200 200]
   :heading 0
   :velocity 10
   :last-position nil
   :state :running
   :ip 0
   :program [:forward :forward :left :forward :forward :left :forward :left :forward]})

(defn setup
  []
  (q/background "white")
  {:automata [(make-automata)]})

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

