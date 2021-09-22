(ns shimmers.common.ui.controls
  (:require [clojure.edn :as edn]
            [goog.dom :as dom]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [shimmers.common.sequence :as cs]))

(defn mount
  "Mounts reagent component to render in an element, defaults to interface.

  Helper method so it can be invoked on run-sketch OR on figwheel reload."
  ([view]
   (mount view "interface"))
  ([view host]
   (rdom/render [view] (dom/getElement host))))

(defn state [m]
  (r/atom m))

(defn assoc-value [settings field-ref]
  (fn [e] (swap! settings assoc-in field-ref
                (edn/read-string (.-target.value e)))))

(defn toggle-value [settings field-ref]
  (fn [_]
    (swap! settings update-in field-ref not)))

(defn change-mode
  ([ui-state modes] (change-mode ui-state modes :mode))
  ([ui-state modes key-name]
   (let [mode (key-name @ui-state)
         cycle-mode #(swap! ui-state update key-name
                            (partial cs/cycle-next modes))]
     [:div
      [:input {:type "button" :value "Cycle Mode"
               :on-click cycle-mode}]
      [:span {:style {:padding-left "1em"}} "Mode: " (name mode)]])))

(defn checkbox [settings label field-ref]
  [:div.label-set {:key label}
   [:input {:type "checkbox" :checked (get-in @settings field-ref)
            :on-change (toggle-value settings field-ref)}]
   [:label label]])

(defn checkbox-after [settings label field-ref]
  [:div.label-set {:key label}
   [:label label]
   [:input {:type "checkbox" :checked (get-in @settings field-ref)
            :on-change (toggle-value settings field-ref)}]])

(defn dropdown [settings label field-ref options]
  (let [selected (get-in @settings field-ref)]
    [:div.label-set {:key (str "dropdown-" field-ref)}
     [:label label]
     [:select {:on-change (fn [e] (swap! settings assoc-in field-ref (.-target.value e)))
               :value selected}
      (for [[name value] options]
        [:option {:key value :value value} name])]]))

(defn slider [settings label-fn field-ref [lower upper step]]
  (let [value (get-in @settings field-ref)]
    [:div.label-set.slider {:key (str "slider-" field-ref)}
     [:label (label-fn value)]
     [:input {:type "range" :value value :min lower :max upper
              :step (or step 1)
              :on-change (assoc-value settings field-ref)}]]))

(defn numeric [settings label field-ref [lower upper step]]
  (let [value (get-in @settings field-ref)]
    [:div.label-set.numeric {:key (str "numeric-" field-ref)}
     [:label label]
     [:input {:type "number" :value value
              :min lower :max upper :step step
              :on-change (assoc-value settings field-ref)}]]))

(defn details [summary & body]
  (into [:details [:summary summary]] body))
