(ns shimmers.common.ui.controls)

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

(defn slider [settings label-fn field-ref [lower upper]]
  (let [value (get-in @settings field-ref)]
    [:div.label-set {:key (str "slider-" field-ref)}
     [:label (label-fn value)]
     [:input {:type "range" :value value :min lower :max upper
              :on-change (fn [e] (swap! settings assoc-in field-ref (int (.-target.value e))))}]]))
