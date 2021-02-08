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
(defn pinwheel [c r speed dir rotations]
  (let [target (* (/ Math/PI 2) rotations)]
    ;; TODO: apply completion effect on grid positions to rotate actual grid
    {:cells [[(dec c) (dec r)] [c (dec r)] [c r] [(dec c) r]]
     :theta 0
     :step
     (fn [effect] (update effect :theta + (* dir speed)))
     :done?
     (fn [{:keys [theta]}]
       (< (Math/abs (- (* dir target) theta)) (* speed 0.5)))
     :on-complete
     (fn [{:keys [grid] :as state} {:keys [cells]}]
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

(defn rotate-row [{:keys [dims]} row n speed]
  (let [[cols _] dims
        dir (if (>= n 0) 1 -1)]
    {:cells (for [c (range cols)] [c row])
     :offset 0
     :step
     (fn [effect] (update effect :offset + speed))
     :done?
     (fn [{:keys [offset]}] (< (- (Math/abs n) offset) (* speed 0.5)))
     :on-complete
     (fn [{:keys [grid] :as state} {:keys [cells]}]
       (let [colors (map (partial get grid) cells)
             cells' (rotate n cells)]
         (assoc state :grid (merge grid (zipmap cells' colors)))))
     :draw
     (fn [{:keys [cells offset]} grid w h]
       ;; FIXME: fill color correctly for missing leading/trailing element
       (q/fill 255)
       (q/rect 0 (* row h) (q/width) h)
       (let [colors (map (partial get grid) cells)]
         (doseq [c (range cols)
                 :let [x (mod (+ c (* dir offset)) cols)
                       color (nth colors (mod c cols))]]
           (apply q/fill color)
           (q/rect (* x w) (* row h) w h)
           ;; (q/fill 255)
           ;; (q/text (str c) (+ (* x w) 30) (+ (* row h) 20))
           ;; (q/text (str x) (+ (* x w) 30) (+ (* row h) 40))
           )))}))

(defn rotate-column [{:keys [dims]} column n speed]
  (let [[_ rows] dims
        dir (if (>= n 0) 1 -1)]
    {:cells (for [r (range rows)] [column r])
     :offset 0
     :step
     (fn [effect] (update effect :offset + speed))
     :done?
     (fn [{:keys [offset]}] (< (- (Math/abs n) offset) (* speed 0.5)))
     :on-complete
     (fn [{:keys [grid] :as state} {:keys [cells]}]
       (let [colors (map (partial get grid) cells)
             cells' (rotate n cells)]
         (assoc state :grid (merge grid (zipmap cells' colors)))))
     :draw
     (fn [{:keys [cells offset]} grid w h]
       ;; FIXME: fill color correctly for missing leading/trailing element
       (q/fill 255)
       (q/rect (* column w) 0 w (q/height))
       (let [colors (map (partial get grid) cells)]
         (doseq [r (range rows)
                 :let [y (mod (+ r (* dir offset)) rows)
                       color (nth colors (mod r rows))]]
           (apply q/fill color)
           (q/rect (* column w) (* y h) w h)
           ;; (q/fill 255)
           ;; (q/text (str c) (+ (* x w) 30) (+ (* row h) 20))
           ;; (q/text (str x) (+ (* x w) 30) (+ (* row h) 40))
           )))}))

(defn make-pinwheel [state]
  (let [[w h] (:dims state)]
    (pinwheel (rand-nth (range 1 w))
              (rand-nth (range 1 h))
              (rand-nth [0.02 0.03 0.04 0.06 0.08 0.10 0.12])
              (if (> (rand) 0.5) 1 -1)
              (rand-nth (range 1 4)))))

(defn make-rotate-row [{:keys [dims] :as state}]
  (let [[cols rows] dims]
    (rotate-row state
                (rand-nth (range rows))
                (* (rand-nth [-1 1])
                   (rand-nth (range 1 cols)))
                (rand-nth [0.02 0.04 0.08]))))

(defn make-rotate-column [{:keys [dims] :as state}]
  (let [[cols rows] dims]
    (rotate-column state
                   (rand-nth (range cols))
                   (* (rand-nth [-1 1])
                      (rand-nth (range 1 rows)))
                   (rand-nth [0.02 0.04 0.08]))))

(defn e-call [msg e]
  ((get e msg) e))

(defn apply-effects [{:keys [effects] :as state}]
  (let [completed (filter (partial e-call :done?) effects)]
    (assoc (reduce (fn [s e] ((:on-complete e) s e)) state completed)
           :effects
           (->> effects
                (remove (set completed))
                (map (partial e-call :step))))))

(defn create-effect [{:keys [effects] :as state}]
  (let [distribution [make-rotate-row
                      make-rotate-column
                      make-pinwheel
                      make-pinwheel
                      make-pinwheel]
        effect ((rand-nth distribution) state)
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
