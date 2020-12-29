(ns shimmers.sketches.probabilistic-automata
  (:require [cljs.core.match :refer-macros [match]]
            [goog.dom :as dom]
            [goog.string.format]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [reagent.dom :as rdom]
            [shimmers.framerate :as framerate]
            [shimmers.math.color :as color]
            [shimmers.automata.simplify :as simplify]
            [shimmers.automata.programs :as programs]))

(defn in-bounds? [[x y] bounds]
  (and (>= x (- bounds)) (< x (+ (q/width) bounds))
       (>= y (- bounds)) (< y (+ (q/height) bounds))))

(def lifespan 1000)
(def max-population 128)
(defn make-automata [position program]
  {:position position
   :heading (* 3 (/ Math/PI 2))
   :last-position nil
   :state :running
   :color [0 0 0 10]
   :ip 0
   :program program})

(defn interpret-argument [arg]
  (match arg
    [:random value] (rand-int value)
    [:gradient value] (color/random-gradient value)
    :else arg))

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
    (->> (concat alive forks)
         (take-last max-population)
         (map execute))))

(defn generate-instruction []
  ((rand-nth
    (programs/weighted
     5 (fn [] [:forward (+ 1 (rand-int 50))])
     2 (fn [] [:rotate (rand (* Math/PI 2))])
     1 (fn [] (programs/rotate 60))
     3 (fn [] [:rotate (- (rand (/ Math/PI 3)) (/ Math/PI 3))]) ;; small angles
     1 (fn [] [:heading (rand (* Math/PI 2))])
     2 (fn [] [:fork 0])
     1 (fn [] [:halt 0])
     3 (fn [] [:color [:gradient :rainbow1]])
     2 (fn [] [:color [0 0 0 10]])
     1 (fn [] [:color [255 255 255 255]])
     1 (fn [] [:one-of (repeatedly (+ 1 (rand-int 5)) generate-instruction)])))))

(defn generate-program
  ([] (->> (fn [] (simplify/simplify-program (generate-program (+ 3 (rand-int 10)))))
           repeatedly
           (filter simplify/accept-program?)
           first))
  ([n] (repeatedly n generate-instruction)))

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
  [:div
   [:p "Program @ " (print-str (:position bot))]
   [:pre {:style {:font-size 10}}
    "[\n" (interpose [:br] (map prettify-instruction (:program bot))) "\n]"]])

(defn render-explanation [automata]
  [:div
   [:p {:style {:width "45em"}}
    "Each of the four visualizations are generated from a corresponding program below.
    Each program is randomly generated from the available instructions, ie a
    subset of the space of possible programs in this language. Sometimes the
    programs are boring or do not create output, so the examples cycle every ~30
    seconds."]
   [:div {:style {:display :grid :grid-template-columns "auto auto"}}
    (map describe automata)]])

(defn setup
  []
  (q/background "white")
  (let [automata [(make-automata [150 100] (generate-program))
                  (make-automata [450 100] (generate-program))
                  (make-automata [150 300] (generate-program))
                  (make-automata [450 300] (generate-program))]]
    (rdom/render [render-explanation automata]
                 (dom/getElement "explanation"))
    {:automata automata}))

(defn update-state
  [state]
  (if (= (mod (inc (q/frame-count)) (* 30 60)) 0)
    (setup)
    (update state :automata execute-all)))

(defn draw-bot [{:keys [position last-position color]}]
  (when (and last-position (not= position last-position))
    (apply q/stroke color)
    (q/line last-position position)))

(defn draw
  [{:keys [automata]}]
  (doseq [bot (filter #(= (:state %) :running) automata)]
    (draw-bot bot)))

(defn ^:export run-sketch []
  (q/defsketch particles
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
