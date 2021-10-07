(ns shimmers.core
  (:require [fipp.edn :as fedn]
            [goog.dom :as dom]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [reitit.coercion.spec :as rss]
            [reitit.frontend :as rf]
            [reitit.frontend.controllers :as rfc]
            [reitit.frontend.easy :as rfe]
            [shimmers.sketches :as sketches]
            [shimmers.view.favicon :as favicon]
            [shimmers.view.index :as view-index]
            [shimmers.view.sketch :as view-sketch]
            [spec-tools.data-spec :as ds]))

;; Uncomment to see javascript source of functions at repl
;; (set! cljs.core/*print-fn-bodies* true)
;; Or just (str the-function)

(defn request->sketch
  "Lookup a sketch by name from request and annotate it with seed if available."
  [{:keys [path query]}]
  (-> path
      :name
      sketches/by-name
      (assoc :seed (:seed query))))

(defn on-event [f action]
  (fn [request]
    (let [sketch (request->sketch request)]
      (when action
        (println action "sketch" (:id sketch)))
      (f sketch))))

;; FIXME: handle invalid paths, re-route to index by-alphabetical
(def routes
  [["/" ::root]
   ["/sketches"
    {:name :shimmers.view.index/by-alphabetical
     :view #(view-index/by-alphabetical (sketches/all))}]
   ["/sketches-by-date"
    {:name :shimmers.view.index/by-date
     :view #(view-index/by-date (sketches/all))}]
   ["/sketches-by-tag"
    {:name :shimmers.view.index/by-tag
     :view #(view-index/by-tag (sketches/all))}]
   ["/sketches/:name"
    {:name :shimmers.view.sketch/sketch-by-name
     :view (on-event #(view-sketch/sketch-by-name % (sketches/known-names)) nil)
     :parameters
     {:path {:name (every-pred string? (set (sketches/known-names)))}
      :query {(ds/opt :seed) int?}}
     :controllers
     [{:parameters {:path [:name] :query [:seed]}
       :start (on-event view-sketch/start-sketch "start")
       :stop (on-event view-sketch/stop-sketch "stop")}]}]])

(defonce match (r/atom nil))

(defn on-navigate [new-match]
  (if (or (nil? new-match) (= (:name (:data new-match)) ::root))
    ;; default route, not sure on reitit for frontend routing
    (rfe/replace-state :shimmers.view.index/by-alphabetical)
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

(defn debug-root []
  [:pre (with-out-str (fedn/pprint @match))])

(defn init []
  (rfe/start!
   ;; coercion here will cause missing sketches to explode
   (rf/router routes {:data {:coercion rss/coercion}})
   on-navigate
   {:use-fragment true})

  ;; kick off favicon animation
  (favicon/start 100)

  ;; (rdom/render [debug-root] (dom/getElement "route-debug-mount"))
  (rdom/render [page-root] (dom/getElement "shimmer-mount")))

;; initialize sketch on first-load
(defonce start-up (init))
