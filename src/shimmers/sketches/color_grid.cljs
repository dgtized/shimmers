(ns shimmers.sketches.color-grid
  (:require [clojure.set :as set]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]))

(defn sample-color [x y]
  [(/ (+ x y) 20) (+ 0.2 (/ x 20)) (+ 0.2 (/ y 12)) 1])

(defn make-grid [cols rows]
  {:effects []
   :dims [cols rows]
   :grid
   (apply merge
          (for [r (range rows)
                c (range cols)]
            {[c r] (sample-color (inc c) (inc r))}))})

(defn rotate
  [n xs]
  (if (>= n 0)
    (->> xs
         cycle
         (drop n)
         (take (count xs)))
    (->> xs
         reverse
         cycle
         (drop (Math/abs n))
         (take (count xs))
         reverse)))

(comment (rotate 1 [1 2 3])
         (rotate -1 [1 2 3]))

;; TODO:
;; Horizontal / vertical slides
;; swap random pair / disolve / teleport?
(defn pinwheel [c r dir rotations]
  (let [target (* (/ Math/PI 2) rotations)]
    ;; TODO: apply completion effect on grid positions to rotate actual grid
    {:cells [[(dec c) (dec r)] [c (dec r)] [c r] [(dec c) r]]
     :theta 0
     :step
     (fn [effect] (update effect :theta + (* dir 0.03)))
     :done?
     (fn [{:keys [theta]}]
       (< (Math/abs (- (* dir target) theta)) 0.05))
     :on-complete
     (fn [{:keys [cells]} {:keys [grid] :as state}]
       (let [colors (map (partial get grid) cells)
             cells' (rotate (* dir rotations) cells)]
         (assoc state :grid (merge grid (zipmap cells' colors)))))
     :draw
     (fn [effect grid w h]
       (let [cells (:cells effect)]
         (q/translate (* c w) (* r h))
         (q/fill 255)
         (q/rect (- w) (- h) (* w 2) (* h 2))
         (q/rotate (:theta effect))
         (apply q/fill (get grid (nth cells 0)))
         (q/rect (- w) (- h) w h)
         (apply q/fill (get grid (nth cells 1)))
         (q/rect 0 (- h) w h)
         (apply q/fill (get grid (nth cells 2)))
         (q/rect 0 0 w h)
         (apply q/fill (get grid (nth cells 3)))
         (q/rect (- w) 0 w h)))}))

(defn make-pinwheel [state]
  (let [[w h] (:dims state)]
    (pinwheel (rand-nth (range 1 w))
              (rand-nth (range 1 h))
              (if (> (rand) 0.5) 1 -1)
              (rand-nth (range 1 4)))))

(defn e-call [msg e]
  ((get e msg) e))

(defn apply-effects [{:keys [effects] :as state}]
  (let [completed (filter (partial e-call :done?) effects)]
    (assoc (reduce (fn [s e] ((:on-complete e) e s)) state completed)
           :effects
           (->> effects
                (remove (set completed))
                (map (partial e-call :step))))))

(defn create-effect [{:keys [effects] :as state}]
  (let [effect (make-pinwheel state)
        avoid-cells (set (mapcat :cells effects))]
    (if (empty? (set/intersection (set (:cells effect)) avoid-cells))
      (update state :effects conj effect)
      state)))

(defn setup []
  (make-grid 12 8))

(defn update-state [state]
  (let [{:keys [effects] :as state'} (apply-effects state)]
    (if (and (< (count effects) 3)
             (< (rand) 0.03))
      (create-effect state')
      state')))

(defn draw [{:keys [grid dims effects]}]
  (q/color-mode :hsl 1 1 1 1)
  (q/rect-mode :corner)
  (let [[cols rows] dims
        w (/ (q/width) cols)
        h (/ (q/height) rows)]
    (doseq [[[c r] color] grid]
      (apply q/fill color)
      (q/rect (* c w) (* r h) w h))
    (doseq [effect effects]
      (q/push-matrix)
      ((:draw effect) effect grid w h)
      (q/pop-matrix))))

(defn ^:export run-sketch []
  (q/defsketch color-grid
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
