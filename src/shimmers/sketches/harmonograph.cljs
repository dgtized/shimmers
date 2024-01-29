(ns shimmers.sketches.harmonograph
  (:require
   [clojure.edn :as edn]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; TODO: add a ctrl/fraction input that accepts and validates:
;; int, float, fraction
;; 2, 1.01, 2.1e4, 1/3, 1.01/3.01
(defn fraction-parse [s]
  (if-let [m (re-find #"^\s*(-?\d+(\.\d*)?)\s*$" s)]
    [(edn/read-string (second m)) nil]
    (if-let [m (re-find #"^\s*(-?\d+(\.\d*)?)\s*/\s*(-?\d+(\.\d*)?)\s*$" s)]
      (let [n (edn/read-string (nth m 1))
            d (edn/read-string (nth m 3))]
        (if (zero? d) ;; divide by zero
          [s "divide by zero"]
          [(/ n d) nil]))
      [s "invalid string"])))

(defn fraction-validate
  ([s] (fraction-validate s 1.0))
  ([s last-value]
   (let [[value error] (fraction-parse s)]
     (if-not error
       {:raw s
        :valid true
        :value value}
       {:raw s
        :valid false
        :error error
        :value (or last-value 1.0)}))))

(defn fraction [settings label field-ref]
  (let [value (get-in @settings field-ref)]
    [:div.label-set.fraction {:key (str "fraction-" field-ref)}
     [:label label]
     [:input {:type "text"
              :value (:raw value)
              :style {:background (if (:error value) "hsl(0,75%,85%)" "")}
              :on-change
              (fn [e]
                (let [v (fraction-validate (.-target.value e) (:value value))]
                  (swap! settings assoc-in field-ref v)))}]
     ;; FIXME: collect this elsewhere somehow
     (when-let [error (:error value)]
       [:div {:style {:color (if (:error value) "hsl(0,75%,50%)" "")}}
        error])
     #_[:span (debug/pre-edn value)]]))

(defonce ui-state
  (ctrl/state {:sample-steps 1000
               :sample-rate 2.0
               :table [(fraction-validate "1 / 1")
                       (fraction-validate "1")
                       (fraction-validate "1")]
               :pendulum [(fraction-validate "1.001 / 3")
                          (fraction-validate "1")
                          (fraction-validate "1")]
               :pen [(fraction-validate "1 / 2")
                     (fraction-validate "1 / 3")]
               :dampen-rate 0.0015
               :dampen-limit 0.2
               :modulate-stroke true
               :weight 0.6
               :pen-modulation false}))

(defn ui-controls []
  [:<>
   [:h3 "Parameters"]
   [:em "(apply after restart)"]
   [:div.grid {:style {:grid-template-columns "0.2fr repeat(3,0.15fr)"
                       :column-gap "2%"}}
    [:div "Table"]
    [fraction ui-state "Ratio" [:table 0]]
    (fraction ui-state "dt-x" [:table 1])
    (fraction ui-state "dt-y" [:table 2])
    [:div "Pendulum"]
    [fraction ui-state "Ratio" [:pendulum 0]]
    (fraction ui-state "dt-x" [:pendulum 1])
    (fraction ui-state "dt-y" [:pendulum 2])
    [:div "Dampen"]
    (ctrl/numeric ui-state "Rate" [:dampen-rate] [0.001 0.01 0.00001])
    (ctrl/numeric ui-state "Limit" [:dampen-limit] [0.01 0.2 0.01])
    [:div {:style {:grid-column "4 / 5" :grid-row "3 / 4"}}]
    [:div "Sample"]
    (ctrl/numeric ui-state "Steps" [:sample-steps] [100 2000 50])
    (ctrl/numeric ui-state "Rate" [:sample-rate] [0.1 12.0 0.1])
    [:div {:style {:grid-column "4 / 5" :grid-row "4 / 5"}}]
    [:div "Stroke"]
    (ctrl/numeric ui-state "Weight" [:weight] [0.1 2.0 0.1])
    (ctrl/checkbox-after ui-state "Modulate" [:modulate-stroke])
    [:div {:style {:grid-column "4 / 5" :grid-row "5 / 6"}}]
    [:div "Pen Stroke"]
    (ctrl/checkbox-after ui-state "Modulation" [:pen-modulation])
    (when (:pen-modulation @ui-state)
      [:<>
       (fraction ui-state "Rate" [:pen 0])
       (fraction ui-state "Phase" [:pen 1])])]

   [view-sketch/generate :harmonograph]])

(defn dampen [lambda t]
  (Math/exp (* (- lambda) t)))

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
            :dpen-phase (:value (second pen))})))

(defn update-state [state]
  (update state :t + 1))

(defn draw
  [{:keys [t sample-steps sample-rate
           dplat table-dxt table-dyt
           dpend pendulum-dxt pendulum-dyt
           dampen-rate dampen-limit
           modulate-stroke weight
           pen-modulation dpen dpen-phase]}]
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
        (apply q/point
               (tm/+ (cq/rel-vec 0.5 0.5)
                     (gv/vec2 (* 0.225 (q/height) k (Math/cos (* table-dxt dplat t)))
                              (* 0.225 (q/height) k (Math/sin (* table-dyt dplat t))))
                     (gv/vec2 (* 0.225 (q/height) k (Math/cos (* pendulum-dxt dpend t)))
                              (* 0.225 (q/height) k (Math/sin (* pendulum-dyt dpend t))))))))))

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
