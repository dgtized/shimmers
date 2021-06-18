(ns shimmers.sketches.flow-fields
  "https://tylerxhobbs.com/essays/2020/flow-fields"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.deterministic-random :as dr]
            [shimmers.math.vector :as v]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(def settings (ctrl/state {:iterations 3
                           :step-size 3}))

(defn dir-at
  [[x y]]
  (* tm/TWO_PI (q/noise (/ x 100) (/ y 100))))

(defn draw-grid [size]
  (let [w (/ (q/width) size)
        h (/ (q/height) size)]
    (doseq [[p dir]
            (for [x (range (* -2 size) (* (+ 3 w) size) size)
                  y (range (* -2 size) (* (+ 3 h) size) size)]
              [(gv/vec2 x y) (dir-at [x y])])]
      (q/line p (v/add p (v/polar (* 0.5 size) dir))))))

(defn next-flow-point [p r]
  (tm/+ p (v/polar r (dir-at p))))

(defn flow-points [p r n]
  (reductions (fn [p] (next-flow-point p r)) p (range n)))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/background 1.0)
  (q/noise-seed (dr/random 1000000))
  (let [{:keys [iterations step-size]} @settings]
    {:iter 0
     :iterations iterations
     :step-size step-size
     :length 32}))

(defn update-state [state]
  (update state :iter inc))

(defn draw [{:keys [step-size length iter iterations]}]
  ;; (q/stroke-weight 0.1)
  ;; (q/stroke 0.0 0.0 0.0 1.0)
  ;; (draw-grid 10)
  (q/stroke-weight 0.2)
  (q/no-fill)
  (q/stroke 0.0 0.0 0.0 1.0)
  (when (< iter iterations)
    (dotimes [_ 1000]
      (q/begin-shape)
      (doseq [[x y] (flow-points (gv/vec2 (cq/rel-pos (dr/random) (dr/random)))
                                 step-size length)]
        (q/curve-vertex x y))
      (q/end-shape))))

(defn explanation []
  [:div
   [:section
    (ctrl/slider settings (fn [v] (str "Iterations " (* 1000 v))) [:iterations] [1 32])
    (ctrl/slider settings (fn [v] (str "Step Size " v)) [:step-size] [1 64])]])

(defn ^:export run-sketch []
  ;; 2021
  (ctrl/mount explanation)
  (q/defsketch flow-fields
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
