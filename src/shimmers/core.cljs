(ns shimmers.core
  (:require [goog.dom :as dom]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [reitit.coercion.spec :as rss]
            [reitit.frontend :as rf]
            [reitit.frontend.controllers :as rfc]
            [reitit.frontend.easy :as rfe]
            [shimmers.common.ui :as ui]
            [shimmers.sketches :as sketches]
            [shimmers.view.index :as view-index]
            [shimmers.view.sketch :as view-sketch]
            [spec-tools.data-spec :as ds]))

;; Uncomment to see javascript source of functions at repl
;; (set! cljs.core/*print-fn-bodies* true)
;; Or just (str the-function)

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
     :view (fn [{:keys [path]}]
             (-> path :name sketches/by-name
                 (view-sketch/sketch-by-name (sketches/known-names))))
     :parameters
     {:path {:name (every-pred string? (set (sketches/known-names)))}
      :query {(ds/opt :seed) int?}}
     :controllers
     [{:parameters {:path [:name] :query [:seed]}
       :start (fn [{:keys [path query]}]
                (let [sketch-name (:name path)
                      sketch (assoc (sketches/by-name sketch-name)
                                    :seed (:seed query))]
                  (println "start" "sketch" sketch-name)
                  (ui/screen-view (name sketch-name))
                  (view-sketch/start-sketch sketch)))
       :stop (fn [{:keys [path]}]
               (println "stop" "sketch" (:name path))
               (view-sketch/stop-sketch))}]}]])

(defonce match (r/atom nil))

(defn on-navigate [new-match]
  (if (or (nil? new-match) (= (:name (:data new-match)) ::root))
    ;; default route, not sure on reitit for frontend routing
    (rfe/replace-state :shimmers.view.sketch/sketch-by-name
                       {:name :superposition})
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
