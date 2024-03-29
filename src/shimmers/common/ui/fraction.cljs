(ns shimmers.common.ui.fraction
  (:require [clojure.edn :as edn]))

(defn check-bounds [v options]
  (if-let [result (:value v)]
    (let [{:keys [lower upper]} options]
      (cond (and upper (> result upper))
            (assoc v :error
                   (str "Value " result " is greater than upper bound " upper "."))
            (and lower (< result lower))
            (assoc v :error
                   (str "Value " result " is below lower bound " lower "."))
            :else v))
    v))

;; TODO: add a ctrl/fraction input that accepts and validates:
;; int, float, fraction
;; 2, 1.01, 2.1e4, 1/3, 1.01/3.01
(defn parse [s]
  (if-let [m (re-find #"^\s*(-?\d+(\.\d*)?)\s*$" s)]
    {:value (edn/read-string (second m))}
    (if-let [m (re-find #"^\s*(-?\d+(\.\d*)?)\s*/\s*(-?\d+(\.\d*)?)\s*$" s)]
      (let [n (edn/read-string (nth m 1))
            d (edn/read-string (nth m 3))]
        (if (zero? d) ;; divide by zero
          {:error "divide by zero"}
          {:value (/ n d)}))
      {:error "invalid string"})))

(defn validate
  ([s] (validate s 1.0 {}))
  ([s last-value options]
   (let [{:keys [value error]} (check-bounds (parse s) options)]
     (if-not error
       {:raw s
        :valid true
        :value value}
       {:raw s
        :valid false
        :error error
        :value (or last-value 1.0)}))))

(defn make [s]
  (validate s))

(defn control [settings label field-ref & options]
  (let [value (get-in @settings field-ref)]
    [:div.label-set.fraction {:key (str "fraction-" field-ref)}
     [:label label]
     [:input {:type "text"
              :value (:raw value)
              :style {:background (if (:error value) "hsl(0,75%,85%)" "")}
              :on-change
              (fn [e]
                (let [v (validate (.-target.value e) (:value value) options)]
                  (swap! settings assoc-in field-ref v)))}]
     ;; FIXME: collect this elsewhere somehow
     (when-let [error (:error value)]
       [:div {:style {:color (if (:error value) "hsl(0,75%,50%)" "")}}
        error])
     #_[:span (debug/pre-edn value)]]))
