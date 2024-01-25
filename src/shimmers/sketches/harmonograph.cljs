(ns shimmers.sketches.harmonograph
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce ui-state
  (ctrl/state {:sample-steps 1000
               :sample-rate 2.0
               :table [1 1 1 1]
               :pendulum [1.001 3 1 1]
               :pen [1 2]
               :pen-phase [1 3]
               :dampen-rate 0.15
               :dampen-limit 0.2
               :modulate-stroke true
               :weight 0.6
               :pen-modulation false}))

(defn ui-controls []
  [:<>
   [:h3 "Parameters"]
   [:em "(apply after restart)"]
   [:div.grid {:style {:grid-template-columns "0.2fr repeat(4,0.125fr)"
                       :column-gap "1%"}}
    [:div "Table Ratio"]
    (ctrl/numeric ui-state "N" [:table 0] [0 32 0.001])
    (ctrl/numeric ui-state "D" [:table 1] [1 16 0.001])
    (ctrl/numeric ui-state "dt-x" [:table 2] [0.001 10 0.001])
    (ctrl/numeric ui-state "dt-y" [:table 3] [0.001 10 0.001])
    [:div "Pendulum Ratio"]
    (ctrl/numeric ui-state "N" [:pendulum 0] [0 32 0.001])
    (ctrl/numeric ui-state "D" [:pendulum 1] [1 16 0.001])
    (ctrl/numeric ui-state "dt-x" [:pendulum 2] [0.001 10 0.001])
    (ctrl/numeric ui-state "dt-y" [:pendulum 3] [0.001 10 0.001])
    [:div "Dampen"]
    (ctrl/numeric ui-state "Rate" [:dampen-rate] [0.01 0.5 0.01])
    (ctrl/numeric ui-state "Limit" [:dampen-limit] [0.01 0.2 0.01])
    [:div {:style {:grid-column "4 / 6" :grid-row "3 / 4"}}]
    [:div "Sample"]
    (ctrl/numeric ui-state "Steps" [:sample-steps] [100 2000 50])
    (ctrl/numeric ui-state "Rate" [:sample-rate] [0.1 12.0 0.1])
    [:div {:style {:grid-column "4 / 6" :grid-row "4 / 5"}}]
    [:div "Stroke"]
    (ctrl/numeric ui-state "Weight" [:weight] [0.1 2.0 0.1])
    (ctrl/checkbox-after ui-state "Modulate" [:modulate-stroke])
    [:div {:style {:grid-column "4 / 6" :grid-row "5 / 6"}}]
    (ctrl/checkbox ui-state "Pen Modulation" [:pen-modulation])
    [:div {:style {:grid-column "2 / 6" :grid-row "6 / 7"}}]
    (when (:pen-modulation @ui-state)
      [:<>
       [:div "Pen Stroke Ratio"]
       (ctrl/numeric ui-state "N" [:pen 0] [0 32 0.001])
       (ctrl/numeric ui-state "D" [:pen 1] [1 16 0.001])
       [:div {:style {:grid-column "4 / 6" :grid-row "7 / 8"}}]
       [:div "Pen Phase Rate"]
       (ctrl/numeric ui-state "N" [:pen-phase 0] [0 32 0.001])
       (ctrl/numeric ui-state "D" [:pen-phase 1] [1 16 0.001])])]

   [view-sketch/generate :harmonograph]])

(defn dampen [lambda t]
  (Math/exp (* (- lambda) t)))

(defn ratio [[a b]]
  (/ (float a) b))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [{:keys [table pendulum pen pen-phase]} @ui-state]
    (merge @ui-state
           {:t 0
            :dplat (ratio table)
            :dpend (ratio pendulum)
            :dpen (ratio pen)
            :dpen-phase (ratio pen-phase)})))

(defn update-state [state]
  (update state :t + 1))

(defn draw
  [{:keys [t sample-steps sample-rate
           dplat dpend
           dampen-rate dampen-limit
           modulate-stroke weight
           pen-modulation dpen dpen-phase]
    [_ _ table-dxt table-dyt] :table
    [_ _ pendulum-dxt pendulum-dyt] :pendulum}]
  (q/stroke-weight weight)
  (dotimes [i sample-steps]
    (let [t (* sample-rate (+ t (/ i sample-steps)))
          k (dampen dampen-rate (* 0.01 t))]
      (if (< k dampen-limit)
        (q/no-loop)
        (q/with-translation
            [(tm/+ (cq/rel-vec 0.5 0.5)
                   (gv/vec2 (* 0.225 (q/height) k (Math/cos (* table-dxt dplat t)))
                            (* 0.225 (q/height) k (Math/sin (* table-dyt dplat t)))))]
          (when modulate-stroke
            (q/stroke-weight (+ weight (* 0.4 (Math/sin (* 2 t))))))
          (when (or (not pen-modulation)
                    (> (Math/sin (+ (* dpen t) (* 2 (Math/sin (* dpen-phase t))))) 0))
            (apply q/point
                   (gv/vec2 (* 0.225 (q/height) k (Math/cos (* pendulum-dxt dpend t)))
                            (* 0.225 (q/height) k (Math/sin (* pendulum-dyt dpend t)))))))))))

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
