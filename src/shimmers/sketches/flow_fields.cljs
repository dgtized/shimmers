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

(def settings
  (ctrl/state {:iterations 3
               :step-size 3
               :stroke-weight 8
               :length 32
               :noise-div 6}))

(defn dir-at
  [[x y] noise-div]
  (* tm/TWO_PI (q/noise (/ x noise-div) (/ y noise-div))))

(defn draw-grid [size noise-div]
  (let [w (/ (q/width) size)
        h (/ (q/height) size)]
    (doseq [[p dir]
            (for [x (range (* -2 size) (* (+ 3 w) size) size)
                  y (range (* -2 size) (* (+ 3 h) size) size)]
              [(gv/vec2 x y) (dir-at [x y] noise-div)])]
      (q/line p (v/add p (v/polar (* 0.5 size) dir))))))

(defn flow-points [p r n noise-div]
  (reductions (fn [p] (tm/+ p (v/polar r (dir-at p noise-div))))
              p (range n)))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/background 1.0)
  (q/noise-seed (dr/random 1000000))
  (let [{:keys [iterations length step-size stroke-weight noise-div]} @settings]
    {:iter 0
     :iterations iterations
     :step-size step-size
     :stroke-weight (/ 1 stroke-weight)
     :noise-div (Math/pow 2 noise-div)
     :length length}))

(defn update-state [state]
  (update state :iter inc))

(defn draw [{:keys [stroke-weight step-size length noise-div
                    iter iterations]}]
  ;; (q/stroke-weight 0.1)
  ;; (q/stroke 0.0 0.0 0.0 1.0)
  ;; (draw-grid 10 noise-div)
  (q/stroke-weight stroke-weight)
  (q/no-fill)
  (q/stroke 0.0 0.0 0.0 1.0)
  (when (< iter iterations)
    (dotimes [_ 1000]
      (q/begin-shape)
      (doseq [[x y] (flow-points (gv/vec2 (cq/rel-pos (dr/random) (dr/random)))
                                 step-size length noise-div)]
        (q/curve-vertex x y))
      (q/end-shape))))

(defn explanation []
  [:div
   [:section
    (ctrl/slider settings (fn [v] (str "Iterations " (* 1000 v))) [:iterations] [1 32])
    (ctrl/slider settings (fn [v] (str "Stroke Weight " (/ 1 v))) [:stroke-weight] [1 64])
    (ctrl/slider settings (fn [v] (str "Step Size " v)) [:step-size] [1 64])
    (ctrl/slider settings (fn [v] (str "Length " v)) [:length] [8 128])
    (ctrl/slider settings (fn [v] (str "Noise Multiplier 1/" (Math/pow 2 v))) [:noise-div] [0 10])]])

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
