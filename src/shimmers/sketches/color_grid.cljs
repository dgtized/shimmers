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

(defn pinwheel [c r]
  {:cells [[(dec c) (dec r)] [(dec c) r] [c (dec r)] [c r]]
   :theta 0
   :delta -0.1})

(defn apply-step [effects]
  (map (fn [effect] (update effect :theta + (:delta effect)))
       effects))

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
            (+ 1 (rand-int (dec h)))))

(defn setup []
  (make-grid 12 8))

(defn update-state [{:keys [effects dims] :as state}]
  (if (empty? effects)
    (update state :effects into (repeatedly 2 (partial create-pinwheel dims)))
    (update state :effects apply-step)))

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
