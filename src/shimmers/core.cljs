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
            [shimmers.sketches :as sketches]
            [spec-tools.data-spec :as ds]))

;; detect window size for initial setup?
(defn fit-window []
  [(/ (.-innerWidth js/window) 2)
   (/ (.-innerHeight js/window) 2)])

(defn known-sketches []
  (sort (map (comp name :id) (sketches/all))))

(defn start-sketch [sketch]
  (when-let [run-sketch (:fn sketch)]
    (apply run-sketch [])))

(defn stop-sketch []
  ;; force active video capture to stop
  (doseq [video (dom/getElementsByTagName "video")]
    (.stop (first (.getTracks (aget video "srcObject")))))
  ;; kill existing sketch at quil-host if present
  (when-let [sketch (q/get-sketch-by-id "quil-host")]
    (q/with-sketch sketch (q/exit)))
  (rdom/unmount-component-at-node (dom/getElement "svg-host"))
  (rdom/unmount-component-at-node (dom/getElement "explanation")))

(defn restart-sketch [sketch]
  (rfe/push-state ::sketch-by-name
                  {:name (:id sketch)}
                  {:seed (rand-int (Math/pow 2 32))}))

(defn cycle-sketch [sketch]
  (let [next-sketch (cs/cycle-next (known-sketches) (name (:id sketch)))]
    (rfe/push-state ::sketch-by-name {:name next-sketch})))

(defn sketch-list []
  (let [sketches (sort-by (comp name :id) (sketches/all))]
    [:section
     [:h1 (str "All Sketches (" (count sketches) ")")]
     (into [:ul]
           (for [sketch sketches]
             [:li [:a {:href (rfe/href ::sketch-by-name {:name (:id sketch)})}
                   (:id sketch)]]))]))

(defn sketch-by-name [{:keys [path]}]
  (let [sketch (sketches/by-name (:name path))]
    [:section {:class "controls"}
     [:span
      [:button {:on-click #(cycle-sketch sketch)} "Next"]
      [:button {:on-click #(restart-sketch sketch)} "Restart"]
      [:button {:on-click #(rfe/push-state ::sketch-list)} "All"]]
     [:span
      [:a {:href (:href (ui/code-link sketch))} (name (:id sketch))]]
     [:span {:id "framerate"}]]))

;; FIXME: handle invalid paths, re-route to sketch-list
(def routes
  [;; "/shimmers"
   ["/" ::root]
   ["/sketches" {:name ::sketch-list :view sketch-list}]
   ["/sketches/:name"
    {:name ::sketch-by-name
     :view sketch-by-name
     :parameters
     {:path {:name (every-pred string? (set (known-sketches)))}
      :query {(ds/opt :seed) int?}}
     :controllers
     [{:parameters {:path [:name] :query [:seed]}
       :start (fn [{:keys [path query]}]
                (let [sketch-name (:name path)
                      sketch (assoc (sketches/by-name sketch-name)
                                    :seed (:seed query))]
                  (println "start" "sketch" sketch-name)
                  (ui/screen-view (name sketch-name))
                  (start-sketch sketch)))
       :stop (fn [{:keys [path]}]
               (println "stop" "sketch" (:name path))
               (stop-sketch))}]}]])

(defonce match (r/atom nil))

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


