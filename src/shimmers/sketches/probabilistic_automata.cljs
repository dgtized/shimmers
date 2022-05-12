(ns shimmers.sketches.probabilistic-automata
  (:require
   [cljs.core.match :refer-macros [match]]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.automata.programs :as programs]
   [shimmers.automata.simplify :as simplify]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.string :as scs]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.color :as color]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]))

(defn in-bounds? [[x y] bounds]
  (and (< (- bounds) x (+ (q/width) bounds))
       (< (- bounds) y (+ (q/height) bounds))))

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
    [:random value] (dr/random value)
    [:gradient value] (color/random-gradient value)
    :else arg))

(defn update-position [{:keys [position heading] :as bot} velocity]
  (let [new-position (tm/+ position (v/polar velocity heading))]
    (if (in-bounds? new-position 100)
      (assoc bot :position new-position)
      (assoc bot :state :halt))))

(defn interpret [{:keys [heading ip program] :as bot} instruction]
  (let [[op argument] instruction
        arg (interpret-argument argument)]
    (case op
      :heading (assoc bot :heading arg)
      :rotate (assoc bot :heading (+ heading arg))
      :forward (update-position bot arg)
      :color (assoc bot :color arg)

      :goto (assoc bot :ip
                   (dec (+ ip
                           (- (count program) (mod ip (count program)))
                           (int arg))))
      :one-of (interpret bot (dr/rand-nth arg))
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
  ((dr/weighted
    {(fn [] [:forward (dr/random-int 1 80)]) 5
     (fn [] [:rotate (dr/random tm/TWO_PI)]) 2
     (fn [] (programs/rotate 60)) 1
     (fn [] [:rotate (dr/random (- (/ Math/PI 3)) (/ Math/PI 3))]) 3 ;; small angles
     (fn [] [:heading (dr/random tm/TWO_PI)]) 1
     (fn [] [:fork 0]) 2
     (fn [] [:halt 0]) 1
     (fn [] [:color [:gradient :rainbow1]]) 3
     (fn [] [:color [0 0 0 32]]) 2
     (fn [] [:color [255 255 255 255]]) 1
     (fn [] [:one-of (repeatedly (dr/random-int 1 6) generate-instruction)]) 1})))

(defn generate-program
  ([] (->> (fn [] (->> (dr/random-int 3 14)
                      generate-program
                      simplify/simplify-program))
           repeatedly
           (filter simplify/accept-program?)
           first))
  ([n] (repeatedly n generate-instruction)))

(defn str-vec [xs]
  (str "[\n" (apply str xs) "\n]"))

(defn prettify-instruction [instruction]
  (let [[op argument] instruction
        arg (if (vector? argument)
              (print-str argument)
              (scs/format "%.1f" argument))]
    (case op
      :one-of
      (print-str [:one-of (->> argument
                               (map prettify-instruction)
                               (map (partial str "\n  "))
                               vec)])
      (print-str [op arg]))))

(defn describe [{:keys [position program]}]
  (let [[x y] position
        pos (str "[" x "," y "]")]
    [:div {:key pos}
     [:p "Program @ " pos]
     [:pre {:style {:font-size 10}}
      (->> program
           (map prettify-instruction)
           (interpose "\n")
           str-vec)]]))

(def instruction-set
  {:forward [:span "Move automata forward by " [:em "n"] " units."]
   :rotate [:span "Rotate automata heading by " [:em "t"] " radians."]
   :heading [:span "Set automata on a specific heading " [:em "t"] " in radians."]
   :color
   [:div "Specify an RGBA color of the pen using a quad like " [:code "[r g b a]"]
    [:div "The argument " [:code "[:gradient :rainbow1]"]
     " selects a random color from a gradient."]]
   :goto [:span "Jump forward " [:em "n"] " instructions, modular to the length of the program."]
   :one-of [:span "Execute a random instruction from a list of instructions."]
   :fork [:span "Create a copy of the current automata running from the next instruction."]
   :halt [:span "Apoptosis for the current automata, removing it from scheduling list."]})

(defn explanation [automata]
  [:div
   [:p.readable-width
    "Each of the four visualizations are generated from a corresponding program below.
    Each program is randomly generated from the available instructions, ie a
    subset of the space of possible programs in this language. Sometimes the
    programs are boring or do not create output, so the examples cycle every ~30
    seconds."]
   [:div {:style {:display :grid :grid-template-columns "auto auto"}}
    (map describe automata)]
   [:p.readable-width
    "The language has 8 instructions. Most instructions take integer or floating
    point argument, or " [:code "[:random n]"] " to select a value at runtime."
    (into [:dl]
          (apply concat
                 (for [[term explanation] instruction-set]
                   [[:dt term]
                    [:dd explanation]])))]])

(defn setup
  []
  (q/background "white")
  (let [automata
        (for [x [0.25 0.75]
              y [0.25 0.75]]
          (make-automata (cq/rel-vec x y) (generate-program)))]
    (ctrl/mount (partial explanation automata) "interface")
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

(defn draw [{:keys [automata]}]
  (doseq [bot (filter #(= (:state %) :running) automata)]
    (draw-bot bot)))

(sketch/defquil probabilistic-automata
  :created-at "2020-11-18"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
