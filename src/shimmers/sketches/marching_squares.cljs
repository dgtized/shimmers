(ns shimmers.sketches.marching-squares
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.marching-squares :as iso]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]
   [thi.ng.strf.core :as f]))

(defonce ui-state
  (ctrl/state {:threshold 0.5
               :divisor 7.0}))

(defn noise [s t x y]
  (q/noise (* x s) (* y s) t))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0.0
   :n 40})

(defn update-state [state]
  (update state :t + 0.002))

(defn draw [{:keys [n t]}]
  (let [sx (/ (q/width) n)
        sy (/ (q/height) n)
        {:keys [threshold divisor]} @ui-state
        m (/ 1 (Math/pow 2 divisor))]
    (doseq [px (tm/norm-range n)]
      (doseq [py (tm/norm-range n)]
        (let [[x y] (cq/rel-vec px py)]
          (q/no-stroke)
          (q/fill (noise m t x y))
          (q/rect x y sx sy)
          (q/no-fill)
          (q/stroke 0.0 1.0)
          (doseq [[p q] (iso/lines [x y] [sx sy] (partial noise m t) threshold)]
            (q/line p q)))))))

(defn ui-controls []
  (ctrl/container
   (ctrl/slider ui-state (fn [v] (f/format ["Divisor 1 / 2 ^ " (f/float 1)] v))
                [:divisor] [5.0 12.0 0.1])
   (ctrl/slider ui-state (fn [v] (str "Threshold " v)) [:threshold] [0.0 1.0 0.01])))

(sketch/defquil marching-squares
  :created-at "2021-09-20"
  :on-mount (fn [] (ctrl/mount ui-controls))
  :size [800 800]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
