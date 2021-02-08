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
    (take (count xs) (drop n (cycle xs)))
    (reverse (take (count xs) (drop (Math/abs n) (cycle (reverse xs)))))))

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

(defn e-call [msg e]
  ((get e msg) e))

(defn apply-step [effects]
  (->> effects
       (remove (partial e-call :done?))
       (map (partial e-call :step))))

(defn apply-effects [state effects]
  (->> effects
       (filter (partial e-call :done?))
       (reduce (fn [s e] ((:on-complete e) e s)) state)))

(defn create-effect [effects [w h]]
  (let [effect (pinwheel (+ 1 (rand-int (dec w)))
                         (+ 1 (rand-int (dec h)))
                         (if (> (rand) 0.5) 1 -1)
                         (+ 1 (rand-int 2)))
        avoid-cells (set (mapcat :cells effects))]
    (if (empty? (set/intersection (set (:cells effect)) avoid-cells))
      [effect]
      [])))

(defn setup []
  (make-grid 12 8))

(defn update-state [{:keys [effects dims] :as state}]
  (let [state' (apply-effects state effects)
        active-effects (apply-step effects)]
    (assoc state' :effects (if (and (< (count active-effects) 3)
                                    (< (rand) 0.03))
                             (into active-effects (create-effect effects dims))
                             active-effects))))

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
