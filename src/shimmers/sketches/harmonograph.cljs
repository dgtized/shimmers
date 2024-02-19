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
   {:simple-harmonograph true
    :sample
    {:steps 1000
     :rate 2.0}
    :table
    {:fx (fraction/make "3/5")
     :fy (fraction/make "2/5")
     :phase (fraction/make "0")}
    :pendulum
    {:fx (fraction/make "4/5")
     :fy (fraction/make "1.01/5")
     :phase (fraction/make "0")}
    :table-pt
    {:ratio (fraction/make "1 / 1")
     :fx (fraction/make "1")
     :fy (fraction/make "1")}
    :pendulum-pt
    {:ratio (fraction/make "1.001 / 3")
     :fx (fraction/make "1")
     :fy (fraction/make "1")}
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

(defn ui-controls [ui-state]
  [:<>
   [:h3 "Parameters"]
   [ctrl/checkbox-after ui-state "Simple Harmonograph" [:simple-harmonograph]]
   [:em "(apply after restart)"]
   (if (:simple-harmonograph @ui-state)
     [:div.grid {:style {:grid-template-columns "0.2fr repeat(3,0.15fr)"
                         :column-gap "2%"}}
      [:div "Table"]
      [fraction/control ui-state "fx" [:table :fx]]
      [fraction/control ui-state "fy" [:table :fy]]
      [fraction/control ui-state "Phase" [:table :phase]]
      [:div "Pendulum"]
      [fraction/control ui-state "fx" [:pendulum :fx]]
      [fraction/control ui-state "fy" [:pendulum :fy]]
      [fraction/control ui-state "Phase" [:pendulum :phase]]
      ]
     [:div.grid {:style {:grid-template-columns "0.2fr repeat(3,0.15fr)"
                         :column-gap "2%"}}
      [:div "Table"]
      [fraction/control ui-state "Ratio" [:table-pt :ratio]]
      [fraction/control ui-state "dt-x" [:table-pt :fx]]
      [fraction/control ui-state "dt-y" [:table-pt :fy]]
      [:div "Pendulum"]
      [fraction/control ui-state "Ratio" [:pendulum-pt :ratio]]
      [fraction/control ui-state "dt-x" [:pendulum-pt :fx]]
      [fraction/control ui-state "dt-y" [:pendulum-pt :fy]]])
   [:div.grid {:style {:grid-template-columns "0.2fr repeat(3,0.15fr)"
                       :column-gap "2%"}}
    [:div "Dampen"]
    (ctrl/numeric ui-state "Rate" [:dampen :rate] [0.001 0.01 0.00001])
    (ctrl/numeric ui-state "Limit" [:dampen :limit] [0.01 0.2 0.01])
    [:div {:style {:grid-column "3 / 4" :grid-row "1 / 3"}}]
    [:div "Sample"]
    [ctrl/numeric ui-state "Steps" [:sample :steps] [100 2000 50]]
    [ctrl/numeric ui-state "Rate" [:sample :rate] [0.1 12.0 0.1]]
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

(defn create-harmonograph
  [{:keys [table pendulum dampen]}]
  (let [A (/ (q/height) 5)
        d (:rate dampen)
        table-period (:value (:phase table))
        pendulum-period (:value (:phase pendulum))]
    (parametric-harmonograph
     (decay-cycle A d (:value (:fx table)) table-period)
     (decay-cycle A d (:value (:fx pendulum)) pendulum-period)
     (decay-cycle A d (:value (:fy table)) table-period)
     (decay-cycle A d (:value (:fy pendulum)) pendulum-period))))

(defn create-pendulum-table
  [{:keys [table-pt pendulum-pt dampen]}]
  (let [A (/ (q/height) 5)
        dplat (:value (:ratio table-pt))
        dpend (:value (:ratio pendulum-pt))
        table-fx (:value (:fx table-pt))
        table-fy (:value (:fy table-pt))
        pendulum-fx (:value (:fx pendulum-pt))
        pendulum-fy (:value (:fy pendulum-pt))
        ]
    (fn [t]
      (let [k (dampening (:rate dampen) t)]
        (tm/* (gv/vec2
               (+ (Math/cos (* table-fx dplat t))
                  (Math/cos (* pendulum-fx dpend t)))
               (+ (Math/sin (* table-fy dplat t))
                  (Math/sin (* pendulum-fy dpend t))))
              (* A k))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [{:keys [simple-harmonograph pen] :as state} @ui-state]
    (merge @ui-state
           {:t 0
            :pen {:modulate (:modulate pen)
                  :rate (:value (:rate pen))
                  :phase (:value (:phase pen))}
            :plot ((if simple-harmonograph
                     create-harmonograph
                     create-pendulum-table)
                   state)})))

(defn update-state [state]
  (update state :t + 1))

(defn draw
  [{:keys [t sample
           dampen stroke pen
           plot]}]
  (q/stroke-weight (:weight stroke))
  (dotimes [i (:steps sample)]
    (let [t (* (:rate sample) (+ t (/ i (:steps sample))))
          k (dampening (:rate dampen) t)]
      (when (< k (:limit dampen))
        (q/no-loop))
      (when (:modulate stroke)
        (q/stroke-weight
         (tm/clamp01 (+ (:weight stroke)
                        (sin-cycle 0.4 (:value (:rate stroke)) 0 t)))))
      (when (or (not (:modulate pen))
                (> (Math/sin (+ (* (:rate pen) t) (:phase pen))) 0))
        (apply q/point (tm/+ (cq/rel-vec 0.5 0.5) (plot t)))))))

(defn page []
  [sketch/with-explanation
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   [ui-controls ui-state]])

(sketch/definition harmonograph
  {:created-at "2024-01-22"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
