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
  (ctrl/state
   {:simple-harmonograph false
    :sample-steps 1000
    :sample-rate 2.0
    :table-b
    {:fx (fraction/make "3/5")
     :fy (fraction/make "2/5")
     :phase (fraction/make "0")}
    :pendulum-b
    {:fx (fraction/make "4/5")
     :fy (fraction/make "1.01/5")
     :phase (fraction/make "0")}
    :table
    [(fraction/make "1 / 1")
     (fraction/make "1")
     (fraction/make "1")]
    :pendulum
    [(fraction/make "1.001 / 3")
     (fraction/make "1")
     (fraction/make "1")]
    :dampen
    {:rate 0.0015
     :limit 0.2}
    :stroke
    {:modulate true
     :weight 0.6
     :rate (fraction/make "2")}
    :pen
    {:modulate false
     :rate (fraction/make "1 / 2")
     :phase (fraction/make "1 / 3")}}))

(defn ui-controls []
  [:<>
   [:h3 "Parameters"]
   [ctrl/checkbox-after ui-state "Simple Harmonograph" [:simple-harmonograph]]
   [:em "(apply after restart)"]
   (if (:simple-harmonograph @ui-state)
     [:div.grid {:style {:grid-template-columns "0.2fr repeat(3,0.15fr)"
                         :column-gap "2%"}}
      [:div "Table"]
      [fraction/control ui-state "fx" [:table-b :fx]]
      [fraction/control ui-state "fy" [:table-b :fy]]
      [fraction/control ui-state "Phase" [:table-b :phase]]
      [:div "Pendulum"]
      [fraction/control ui-state "fx" [:pendulum-b :fx]]
      [fraction/control ui-state "fy" [:pendulum-b :fy]]
      [fraction/control ui-state "Phase" [:pendulum-b :phase]]
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
    (ctrl/numeric ui-state "Rate" [:dampen :rate] [0.001 0.01 0.00001])
    (ctrl/numeric ui-state "Limit" [:dampen :limit] [0.01 0.2 0.01])
    [:div {:style {:grid-column "3 / 4" :grid-row "1 / 3"}}]
    [:div "Sample"]
    [ctrl/numeric ui-state "Steps" [:sample-steps] [100 2000 50]]
    [ctrl/numeric ui-state "Rate" [:sample-rate] [0.1 12.0 0.1]]
    [:div {:style {:grid-column "3 / 4" :grid-row "2 / 3"}}]
    [:div "Stroke Weight"]
    [ctrl/checkbox-after ui-state "Modulate" [:stroke :modulate]]
    [ctrl/numeric ui-state "W" [:stroke :weight] [0.1 2.0 0.1]]
    (if (:modulate (:stroke @ui-state))
      [fraction/control ui-state "Rate" [:stroke :rate]]
      [:div])
    [:div "Pen Up/Down"]
    [ctrl/checkbox-after ui-state "Modulate" [:pen :modulate]]
    (when (:modulate (:pen @ui-state))
      [:<>
       [fraction/control ui-state "Rate" [:pen :rate]]
       [fraction/control ui-state "Phase" [:pen :phase]]])]

   [view-sketch/generate :harmonograph]])

(defn dampening [lambda t]
  (Math/exp (* (- lambda) t)))

(defn sin-cycle [amplitude frequency period t]
  (* amplitude (Math/sin (+ (* frequency t) period))))

;; TODO: Add the real parametric equation from: https://en.wikipedia.org/wiki/Harmonograph
(defn decay-cycle [amplitude decay frequency period]
  (fn [t]
    (* (dampening decay t)
       (sin-cycle amplitude frequency period t))))

(defn parametric-harmonograph [xa xb ya yb]
  (fn [t]
    (gv/vec2 (+ (xa t) (xb t))
             (+ (ya t) (yb t)))))

(defn create-harmonograph [{:keys [table-b pendulum-b dampen]}]
  (let [A (/ (q/height) 5)
        d (:rate dampen)
        table-period (:value (:phase table-b))
        pendulum-period (:value (:phase pendulum-b))]
    (parametric-harmonograph
     (decay-cycle A d (:value (:fx table-b)) table-period)
     (decay-cycle A d (:value (:fx pendulum-b)) pendulum-period)
     (decay-cycle A d (:value (:fy table-b)) table-period)
     (decay-cycle A d (:value (:fy pendulum-b)) pendulum-period))))

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
            :pen {:modulate (:modulate pen)
                  :rate (:value (:rate pen))
                  :phase (:value (:phase pen))}
            :harmonograph (create-harmonograph @ui-state)
            })))

(defn update-state [state]
  (update state :t + 1))

(defn draw
  [{:keys [t sample-steps sample-rate
           dampen stroke pen
           harmonograph]
    :as state}]
  (q/stroke-weight (:weight stroke))
  (dotimes [i sample-steps]
    (let [t (* sample-rate (+ t (/ i sample-steps)))
          k (dampening (:rate dampen) t)]
      (when (< k (:limit dampen))
        (q/no-loop))
      (when (:modulate stroke)
        (q/stroke-weight
         (tm/clamp01 (+ (:weight stroke)
                        (sin-cycle 0.4 (:value (:rate stroke)) 0 t)))))
      (when (or (not (:modulate pen))
                (> (Math/sin (+ (* (:rate pen) t) (:phase pen))) 0))
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
