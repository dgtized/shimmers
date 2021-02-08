(ns shimmers.sketches.color-grid
  (:require [quil.core :as q :include-macros true]
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

(defn pinwheel [c r dir]
  (let [target (* (/ Math/PI 2) (+ 1 (rand-int 11)))]
    ;; TODO: apply completion effect on grid positions to rotate actual grid
    {:cells [[(dec c) (dec r)] [c (dec r)] [c r] [(dec c) r]]
     :theta 0
     :step (fn [effect] (update effect :theta + (* dir 0.03)))
     :complete (fn [{:keys [theta]}]
                 (< (Math/abs (- (* dir target) theta)) 0.05))}))

(defn apply-step [effects]
  (->> effects
       (remove (fn [effect] ((:complete effect) effect)))
       (map (fn [effect] ((:step effect) effect)))))

(defn draw-step [grid effect w h]
  (let [cells (:cells effect)
        p (last cells)
        [c r] p]
    (q/translate (* c w) (* r h))
    (q/rotate (:theta effect))
    (apply q/fill (get grid (nth cells 0)))
    (q/rect (- w) (- h) w h)
    (apply q/fill (get grid (nth cells 1)))
    (q/rect (- w) 0 w h)
    (apply q/fill (get grid (nth cells 2)))
    (q/rect 0 (- h) w h)
    (apply q/fill (get grid (nth cells 3)))
    (q/rect 0 0 w h)))

(defn create-pinwheel [[w h]]
  ;; note this should check for collisions with effects or another pinwheel
  (pinwheel (+ 1 (rand-int (dec w)))
            (+ 1 (rand-int (dec h)))
            (if (> (rand) 0.5) 1 -1)))

(defn setup []
  (make-grid 12 8))

(defn update-state [{:keys [effects dims] :as state}]
  (let [active-effects (apply-step effects)]
    (assoc state :effects (if (and (< (count active-effects) 3)
                                   (< (rand) 0.1))
                            (conj active-effects (create-pinwheel dims))
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
      (draw-step grid effect w h)
      (q/pop-matrix))))

(defn ^:export run-sketch []
  (q/defsketch color-grid
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
