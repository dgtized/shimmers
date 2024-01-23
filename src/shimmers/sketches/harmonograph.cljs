(ns shimmers.sketches.harmonograph
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]))

(defonce ui-state
  (ctrl/state {:table-n 1.0
               :table-d 3.0
               :pendulum-n 1.01
               :pendulum-d 2.0
               :dampen-limit 0.05}))

(defn ui-controls []
  (ctrl/container
   [:h3 "Parameters"]
   [:div.flexcols
    [:div {:style {:width "8em"}} "Table Ratio"]
    (ctrl/numeric ui-state "N" [:table-n] [0 32 0.001])
    (ctrl/numeric ui-state "D" [:table-d] [1 16 0.1])]
   [:div.flexcols
    [:div {:style {:width "8em"}} "Pendulum Ratio"]
    (ctrl/numeric ui-state "N" [:pendulum-n] [0 32 0.001])
    (ctrl/numeric ui-state "D" [:pendulum-d] [1 16 0.1])]
   [:div {:style {:width "16em"}}
    (ctrl/numeric ui-state "Dampen Limit" [:dampen-limit] [0.01 0.2 0.01])]
   [:em "(updates after restart)"]))

(defn dampen [lambda t]
  (Math/exp (* (- lambda) t)))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [{:keys [table-n table-d pendulum-n pendulum-d
                dampen-limit]}
        @ui-state]
    {:t 0
     :dplat (/ table-n table-d)
     :dpend (/ pendulum-n pendulum-d)
     :dampen-limit dampen-limit}))

(defn update-state [state]
  (update state :t + 1))

(defn modular-stroke [t]
  (q/stroke-weight (+ 1.2 (* 0.6 (Math/sin (* 2 t))))))

(defn skip-draw [k t]
  (when (> (Math/sin (+ (* (/ 7 5) t) (* 3 (Math/sin (* (/ 1 5) t))))) 0)
    (modular-stroke t)
    (apply q/point (v/polar (* 0.3 (q/height) k) (* (/ 1 6) t)))))

(defn draw [{:keys [t dplat dpend dampen-limit]}]
  (q/stroke-weight 0.33)
  (dotimes [i 1000]
    (let [t (+ (* 4.0 t) (/ i 200))
          k (dampen 0.15 (* 0.01 t))]
      (if (< k dampen-limit)
        (q/no-loop)
        (q/with-translation
          [(tm/+ (cq/rel-vec 0.5 0.5)
                 (v/polar (* 0.15 (q/height) k) (* dplat t)))]
          (apply q/point (v/polar (* 0.3 (q/height) k) (* dpend t))))))))

(defn page []
  [sketch/with-explanation
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [ui-controls]])

(sketch/definition harmonograph
  {:created-at "2024-01-22"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
