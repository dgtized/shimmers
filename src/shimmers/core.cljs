(ns shimmers.core
  (:require [goog.dom :as dom]
            [quil.core :as q :include-macros true]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [reitit.coercion.spec :as rss]
            [reitit.frontend :as rf]
            [reitit.frontend.controllers :as rfc]
            [reitit.frontend.easy :as rfe]
            [shimmers.common.sequence :as cs]
            [shimmers.common.ui :as ui]
            [shimmers.sketches :as sketches]))

;; detect window size for initial setup?
(defn fit-window []
  [(/ (.-innerWidth js/window) 2)
   (/ (.-innerHeight js/window) 2)])

(defn init-sketches [sketches]
  (atom {:sketches (sort-by (comp name :id) sketches)
         :current nil}))

(defonce state (init-sketches (sketches/all)))

(defn current-sketch []
  (let [{:keys [sketches current]} @state]
    (first (filter #(= current (:id %)) sketches))))

(defn run-current []
  (when-let [sketch (current-sketch)]
    (apply (:fn sketch) [])))

(defn stop-sketch []
  ;; force active video capture to stop
  (doseq [video (dom/getElementsByTagName "video")]
    (.stop (first (.getTracks (aget video "srcObject")))))
  ;; kill existing sketch at quil-host if present
  (when-let [sketch (q/get-sketch-by-id "quil-host")]
    (q/with-sketch sketch (q/exit)))
  (rdom/unmount-component-at-node (dom/getElement "svg-host"))
  (rdom/unmount-component-at-node (dom/getElement "explanation")))

(defn restart-sketch []
  (stop-sketch)
  (run-current))

(defn cycle-sketch []
  (let [{:keys [sketches current]} @state
        next-sketch (cs/cycle-next (map :id sketches) current)]
    (rfe/push-state ::sketch-by-name {:name next-sketch})))

(defonce match (r/atom nil))

(defn sketch-list []
  (let [{:keys [sketches]} @state]
    [:section
     [:h1 (str "All Sketches (" (count sketches ) ")")]
     (into [:ul]
           (for [sketch sketches]
             [:li [:a {:href (rfe/href ::sketch-by-name {:name (:id sketch)})}
                   (:id sketch)]]))]))

(defn sketch-by-name []
  (let [active (current-sketch)]
    [:section {:class "controls"}
     [:span
      [:button {:on-click cycle-sketch} "Next"]
      [:button {:on-click restart-sketch} "Restart"]
      [:button {:on-click #(rfe/push-state ::sketch-list)} "All"]]
     [:span
      [:a {:href (:href (ui/code-link active))} (name (:id active))]]
     [:span {:id "framerate"}]]))

(defn known-sketches []
  (map (comp name :id) (get @state :sketches)))

(def routes
  [;; "/shimmers"
   ["/" ::root]
   ["/sketches" {:name ::sketch-list :view sketch-list}]
   ["/sketches/:name"
    {:name ::sketch-by-name
     :view sketch-by-name
     :parameters
     {:path {:name (every-pred string? (set (known-sketches)))}}
     :controllers
     [{:parameters {:path [:name]}
       :start (fn [{:keys [path]}]
                (let [sketch-name (:name path)]
                  (println "start" "sketch" sketch-name)
                  (ui/screen-view (name sketch-name))
                  (swap! state assoc :current (keyword sketch-name))
                  (run-current)))
       :stop (fn [{:keys [path]}]
               (println "stop" "sketch" (:name path))
               (stop-sketch))}]}]])

(defn on-navigate [new-match]
  (if (or (nil? new-match) (= (:name (:data new-match)) ::root))
    ;; default route, not sure on reitit for frontend routing
    (rfe/replace-state ::sketch-by-name {:name :superposition})
    (swap! match
           (fn [old-match]
             (if new-match
               (assoc new-match :controllers
                      (rfc/apply-controllers (:controllers old-match) new-match))
               old-match)))))

(defn page-root []
  (let [page @match
        view (:view (:data page))]
    (when view
      [view (:parameters page)])))

(defn init []
  (rfe/start!
   ;; coercion here will cause missing sketches to explode
   (rf/router routes {:data {:coercion rss/coercion}})
   on-navigate
   {:use-fragment true})

  (rdom/render [page-root] (dom/getElement "shimmer-mount")))

;; initialize sketch on first-load
(defonce start-up (init))


