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
   [thi.ng.math.core :as tm]
   [shimmers.common.ui.fraction :as fraction]))

(defonce ui-state
  (ctrl/state {:simple-harmonograph false
               :sample-steps 1000
               :sample-rate 2.0
               :table-b [(fraction/validate "2")
                         (fraction/validate "4")
                         (fraction/validate "0")]
               :pendulum-b [(fraction/validate "3")
                            (fraction/validate "1.01")
                            (fraction/validate "0")]
               :table [(fraction/validate "1 / 1")
                       (fraction/validate "1")
                       (fraction/validate "1")]
               :pendulum [(fraction/validate "1.001 / 3")
                          (fraction/validate "1")
                          (fraction/validate "1")]
               :pen [(fraction/validate "1 / 2")
                     (fraction/validate "1 / 3")]
               :dampen-rate 0.0015
               :dampen-limit 0.2
               :modulate-stroke true
               :weight 0.6
               :pen-modulation false}))

(defn ui-controls []
  [:<>
   [:h3 "Parameters"]
   [ctrl/checkbox-after ui-state "Simple Harmonograph" [:simple-harmonograph]]
   [:em "(apply after restart)"]
   (if (:simple-harmonograph @ui-state)
     [:div.grid {:style {:grid-template-columns "0.2fr repeat(3,0.15fr)"
                         :column-gap "2%"}}
      [:div "Table"]
      [fraction/control ui-state "A" [:table-b 0]]
      [fraction/control ui-state "B" [:table-b 1]]
      [fraction/control ui-state "Phase" [:table-b 2]]
      [:div "Pendulum"]
      [fraction/control ui-state "A" [:pendulum-b 0]]
      [fraction/control ui-state "B" [:pendulum-b 1]]
      [fraction/control ui-state "Phase" [:pendulum-b 2]]
      ]
     [:div.grid {:style {:grid-template-columns "0.2fr repeat(3,0.15fr)"
                         :column-gap "2%"}}
      [:div "Table"]
      [fraction/control ui-state "Ratio" [:table 0]]
      [fraction/control ui-state "dt-x" [:table 1]]
      [fraction/control ui-state "dt-y" [:table 2]]
      [:div "Pendulum"]
      [fraction/control ui-state "Ratio" [:pendulum 0]]
      [fraction/control ui-state "dt-x" [:pendulum 1]]
      [fraction/control ui-state "dt-y" [:pendulum 2]]])
   [:div.grid {:style {:grid-template-columns "0.2fr repeat(3,0.15fr)"
                       :column-gap "2%"}}
    [:div "Dampen"]
    (ctrl/numeric ui-state "Rate" [:dampen-rate] [0.001 0.01 0.00001])
    (ctrl/numeric ui-state "Limit" [:dampen-limit] [0.01 0.2 0.01])
    [:div {:style {:grid-column "3 / 4" :grid-row "1 / 3"}}]
    [:div "Sample"]
    [ctrl/numeric ui-state "Steps" [:sample-steps] [100 2000 50]]
    [ctrl/numeric ui-state "Rate" [:sample-rate] [0.1 12.0 0.1]]
    [:div {:style {:grid-column "3 / 4" :grid-row "2 / 3"}}]
    [:div "Stroke"]
    [ctrl/numeric ui-state "Weight" [:weight] [0.1 2.0 0.1]]
    [ctrl/checkbox-after ui-state "Modulate" [:modulate-stroke]]
    [:div {:style {:grid-column "3 / 4" :grid-row "3 / 3"}}]
    [:div "Pen Stroke"]
    [ctrl/checkbox-after ui-state "Modulation" [:pen-modulation]]
    (when (:pen-modulation @ui-state)
      [:<>
       [fraction/control ui-state "Rate" [:pen 0]]
       [fraction/control ui-state "Phase" [:pen 1]]])]

   [view-sketch/generate :harmonograph]])

(defn dampen [lambda t]
  (Math/exp (* (- lambda) t)))

;; TODO: Add the real parametric equation from: https://en.wikipedia.org/wiki/Harmonograph
(defn decay-cycle [amplitude decay frequency period]
  (fn [t]
    (* amplitude
       (Math/exp (* (- decay) t))
       (Math/sin (+ (* frequency t) period)))))

(defn parametric-harmonograph [xa xb ya yb]
  (fn [t]
    (gv/vec2 (+ (xa t) (xb t))
             (+ (ya t) (yb t)))))

(defn create-harmonograph [{:keys [table-b pendulum-b dampen-rate]}]
  (let [A (/ (q/height) 5)
        d dampen-rate
        table-period (:value (nth table-b 2))
        pendulum-period (:value (nth pendulum-b 2))]
    (parametric-harmonograph
     (decay-cycle A d (:value (nth table-b 0)) table-period)
     (decay-cycle A d (:value (nth pendulum-b 0)) pendulum-period)
     (decay-cycle A d (:value (nth table-b 1)) table-period)
     (decay-cycle A d (:value (nth pendulum-b 1)) pendulum-period))))

(defn hgraph
  [{:keys [dplat table-dxt table-dyt
           dpend pendulum-dxt pendulum-dyt]} k t]
  (let [size (* 0.225 (q/height) k)]
    (tm/+ (gv/vec2 (* size (Math/cos (* table-dxt dplat t)))
                   (* size (Math/sin (* table-dyt dplat t))))
          (gv/vec2 (* size (Math/cos (* pendulum-dxt dpend t)))
                   (* size (Math/sin (* pendulum-dyt dpend t)))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [{:keys [table pendulum pen]} @ui-state]
    (merge @ui-state
           {:t 0
            :dplat (:value (nth table 0))
            :table-dxt (:value (nth table 1))
            :table-dyt (:value (nth table 2))
            :dpend (:value (nth pendulum 0))
            :pendulum-dxt (:value (nth pendulum 1))
            :pendulum-dyt (:value (nth pendulum 2))
            :dpen (:value (first pen))
            :dpen-phase (:value (second pen))
            :harmonograph (create-harmonograph @ui-state)
            })))

(defn update-state [state]
  (update state :t + 1))

(defn draw
  [{:keys [t sample-steps sample-rate
           dampen-rate dampen-limit
           modulate-stroke weight
           pen-modulation dpen dpen-phase
           harmonograph]
    :as state}]
  (q/stroke-weight weight)
  (dotimes [i sample-steps]
    (let [t (* sample-rate (+ t (/ i sample-steps)))
          k (dampen dampen-rate t)]
      (when (< k dampen-limit)
        (q/no-loop))
      (when modulate-stroke
        (q/stroke-weight (+ weight (* 0.4 (Math/sin (* 2 t))))))
      (when (or (not pen-modulation)
                (> (Math/sin (+ (* dpen t) (* 2 (Math/sin (* dpen-phase t))))) 0))
        (let [pos (if (:simple-harmonograph state)
                    (harmonograph t)
                    (hgraph state k t))]
          (apply q/point (tm/+ (cq/rel-vec 0.5 0.5) pos)))))))

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
