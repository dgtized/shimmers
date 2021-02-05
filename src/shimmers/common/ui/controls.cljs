(ns shimmers.common.ui.controls)

(defn checkbox [settings label field-ref]
  [:div.label-set {:key label}
   [:input {:type "checkbox" :checked (get-in @settings field-ref)
            :on-change #(swap! settings update-in field-ref not)}]
   [:label label]])
