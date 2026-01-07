(ns shimmers.common.ui.controls
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [goog.dom :as dom]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [shimmers.common.sequence :as cs]))

(defn mount
  "Mounts reagent component to render in an element, defaults to interface.

  Helper method so it can be invoked on run-sketch OR on figwheel reload."
  ([view]
   (mount view "sketch-host"))
  ([view host]
   (rdom/render [view] (dom/getElement host))))

(defn state [m]
  (r/atom m))

(defn assoc-value
  ([settings field-ref reader]
   (assoc-value settings field-ref reader (fn [])))
  ([settings field-ref reader on-change]
   (fn [e]
     (swap! settings assoc-in field-ref
            (reader (.-target.value e)))
     (when on-change
       (on-change)))))

(defn toggle-value [settings field-ref]
  (fn [_]
    (swap! settings update-in field-ref not)))

(defn toggle-value-on-change [settings field-ref on-change]
  (fn [_]
    (swap! settings update-in field-ref not)
    (on-change (get-in @settings field-ref))))

(defn container [& body]
  (let [[params body] (if (map? (first body))
                        [(first body) (rest body)]
                        [{} body])]
    (into [:div.ui-controls params] (keep identity body))))

(defn change-mode
  ([ui-state modes]
   (change-mode ui-state modes {:mode-key :mode}))
  ([ui-state modes
    {:keys [button-value mode-desc mode-key on-change]
     :or {button-value "Cycle Mode"
          mode-desc "Mode: "
          mode-key :mode
          on-change (fn [])}}]
   (let [mode (mode-key @ui-state)
         cycle-mode #(do (swap! ui-state update mode-key
                                (partial cs/cycle-next modes))
                         (on-change))]
     [:div
      [:input {:type "button" :value button-value
               :on-click cycle-mode}]
      [:span {:style {:padding-left "1em"}} mode-desc (name mode)]])))

(defn toggle-button
  [ui-state
   {:keys [states mode-key on-change]
    :or {states {false "Paused" true "Playing"}
         on-change (fn [])}}]
  (let [mode (get @ui-state mode-key)
        toggle (fn []
                 (swap! ui-state update mode-key not)
                 (on-change))]
    [:div
     [:input {:type "button"
              :value (str (get states mode))
              :on-click toggle}]]))

(defn checkbox
  ([settings label field-ref]
   (checkbox settings label field-ref {}))
  ([settings label field-ref
    {:keys [on-change] :or {on-change (fn [])}}]
   [:div.label-set {:key label}
    [:input {:type "checkbox" :checked (get-in @settings field-ref)
             :on-change (toggle-value-on-change settings field-ref on-change)}]
    [:label label]]))

(defn checkbox-after [settings label field-ref]
  [:div.label-set {:key label}
   [:label label]
   [:input {:type "checkbox" :checked (get-in @settings field-ref)
            :on-change (toggle-value settings field-ref)}]])

(defn dropdown
  ([settings label field-ref options]
   (dropdown settings label field-ref options nil))
  ([settings label field-ref options {:keys [on-change]}]
   [:div.label-set.dropdown {:key (str "dropdown-" field-ref)}
    [:label label]
    [:select {:on-change (assoc-value settings field-ref identity on-change)
              :value (get-in @settings field-ref)}
     (for [[name value] options]
       [:option {:key value :value value} name])]]))

(defn slider [settings label-fn field-ref [lower upper step]]
  (let [value (get-in @settings field-ref)]
    [:div.label-set.slider {:key (str "slider-" field-ref)}
     [:label (if (ifn? label-fn) (label-fn value) label-fn)]
     [:input {:type "range" :value value :min lower :max upper
              :step (or step 1)
              :on-change (assoc-value settings field-ref edn/read-string)}]]))

(defn numeric
  ([settings label field-ref step-range]
   (numeric settings label field-ref step-range {}))
  ([settings label field-ref [lower upper step] {:keys [on-change]}]
   [:div.label-set.numeric {:key (str "numeric-" field-ref)}
    [:label label]
    [:input {:type "number" :min lower :max upper :step step
             :value (get-in @settings field-ref)
             :on-change (assoc-value settings field-ref edn/read-string on-change)}]]))

(defn color [settings label field-ref]
  [:div.label-set.color {:key (str "color-" field-ref)}
   [:label label]
   [:input {:type "color"
            :value (get-in @settings field-ref)
            :on-change (assoc-value settings field-ref identity)}]])

(defn palette-colors [settings label field-ref]
  (let [colors (str/split (get-in @settings field-ref) #",")]
    [:div.label-set.palette {:key (str "colors-" field-ref)}
     [:label label]
     (into [:div]
           (for [[idx color] (map-indexed vector colors)]
             [:span.color [:input {:type "color"
                                   :value color
                                   :on-change
                                   (fn [e] (swap! settings assoc-in field-ref
                                                 (str/join "," (assoc colors idx (.-target.value e)))))}]
              (when (> (count colors) 1)
                [:button.remove-palette
                 {:on-click
                  (fn [] (swap! settings assoc-in field-ref
                               (str/join "," (concat (take idx colors)
                                                     (drop (inc idx) colors)))))}
                 [:sup "x"]])]))
     (when (< (count colors) 6)
       [:button.add-palette
        {:on-click
         (fn [] (swap! settings assoc-in field-ref
                      (str/join "," (conj colors "#000000"))))}
        "+"])]))

(defn details [summary & body]
  (into [:details [:summary summary]] body))

;; Cribbed from https://gist.github.com/PlumpMath/66ad1d1654597056bbdde24b9808a883
;; and http://timothypratley.blogspot.com/2017/01/reagent-deep-dive-part-2-lifecycle-of.html
#_{:clj-kondo/ignore [:unused-binding]}
(defn canvas [attributes render-frame-fn]
  (let [!canvas (atom nil)]
    (r/create-class
     {:display-name "control-canvas"
      :component-did-mount
      (fn [this]
        (r/set-state this {:active true})
        (render-frame-fn this @!canvas))
      :component-will-unmount
      (fn [this]
        (r/set-state this {:active false}))
      :reagent-render
      (fn [attributes _]
        [:canvas (assoc attributes :ref (fn [el] (reset! !canvas el)))])})))
