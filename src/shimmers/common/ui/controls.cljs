(ns shimmers.common.ui.controls
  (:require [goog.dom :as dom]
            [reagent.dom :as rdom]
            [shimmers.common.sequence :as cs]))

(defn mount
  "Mounts reagent component to render in an element, defaults to explanation.

  Helper method so it can be invoked on run-sketch OR on figwheel reload."
  ([view]
   (mount view "explanation"))
  ([view host]
   (rdom/render [view] (dom/getElement host))))

(defn change-mode [ui-state modes]
  (let [mode (:mode @ui-state)
        cycle-mode #(swap! ui-state update :mode
                           (partial cs/cycle-next modes))]
    [:div
     [:input {:type "button" :value "Cycle Mode"
              :on-click cycle-mode}]
     [:span {:style {:padding-left "1em"}} "Mode: " (name mode)]]))

(defn checkbox [settings label field-ref]
  [:div.label-set {:key label}
   [:input {:type "checkbox" :checked (get-in @settings field-ref)
            :on-change #(swap! settings update-in field-ref not)}]
   [:label label]])

(defn dropdown [settings label field-ref selected-fn options]
  (let [selected (get-in @settings field-ref)]
    [:div.label-set {:key (str "dropdown-" field-ref)}
     [:label label]
     [:select {:on-change (fn [e] (swap! settings assoc-in field-ref (.-target.value e)))}
      (for [[name value] options]
        [:option (if (selected-fn selected value)
                   {:key value :value value :selected true}
                   {:key value :value value})
         name])]]))

(defn slider [settings label-fn field-ref [lower upper step]]
  (let [value (get-in @settings field-ref)]
    [:div.label-set.slider {:key (str "slider-" field-ref)}
     [:label (label-fn value)]
     [:input {:type "range" :value value :min lower :max upper
              :step (or step 1)
              :on-change (fn [e] (swap! settings assoc-in field-ref (int (.-target.value e))))}]]))
