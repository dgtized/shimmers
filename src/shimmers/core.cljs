(ns shimmers.core
  (:require
   [goog.dom :as dom]
   [reagent-keybindings.keyboard :as kb]
   [reagent.core :as r]
   [reagent.dom.client :as rdomc]
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
        (println action "sketch" (:sketch-id sketch)))
      (f sketch))))

;; FIXME: moving this to a different namespace seems to break routing
;; index does not load and p5 is not required somehow
;; FIXME: handle invalid paths, re-route to index by-alphabetical
(def routes
  (let [on-index
        [{:start #(favicon/start 333)
          :stop #(favicon/stop)}]]
    [["/" ::root]
     ["/sketches"
      {:name :shimmers.view.index/by-alphabetical
       :view #(view-index/by-alphabetical (sketches/all))
       :controllers on-index}]
     ["/sketches-by-date"
      {:name :shimmers.view.index/by-date
       :view #(view-index/by-date (sketches/all))
       :controllers on-index}]
     ["/sketches-by-tag"
      {:name :shimmers.view.index/by-tag
       :view #(view-index/by-tag (sketches/all))
       :controllers on-index}]
     ["/sketches/:name"
      {:name :shimmers.view.sketch/sketch-by-name
       :view (on-event #(view-sketch/sketch-by-name % (sketches/known-names)) nil)
       :parameters
       {:path {:name (every-pred string? (set (sketches/known-names)))}
        :query {(ds/opt :seed) int?}}
       :controllers
       [{:parameters {:path [:name] :query [:seed]}
         :start (on-event view-sketch/start-sketch "start")
         :stop (on-event view-sketch/stop-sketch "stop")}]}]]))

(defn on-navigate [page-match new-match]
  (if (or (nil? new-match) (= (:name (:data new-match)) ::root))
    ;; default route, not sure on reitit for frontend routing
    (rfe/replace-state :shimmers.view.index/by-alphabetical)
    (swap! page-match
           (fn [old-match]
             (if new-match
               (assoc new-match :controllers
                      (rfc/apply-controllers (:controllers old-match) new-match))
               old-match)))))

(defn allow-reload-save-keybindings []
  (reset! kb/preventing-default-keys []))

(defn page-root [page-match]
  (let [page @page-match
        view (:view (:data page))]
    (when view
      [:div
       [kb/keyboard-listener]
       [view (:parameters page)]])))

(defonce shimmer-root (rdomc/create-root (dom/getElement "shimmer-mount")))
(defonce !active-route (r/atom nil))

(defn init []
  (rfe/start!
   ;; coercion here will cause missing sketches to explode
   (rf/router routes {:data {:coercion rss/coercion}})
   (partial on-navigate !active-route)
   {:use-fragment true})

  ;; Render at least one frame of the favicon animation at start
  (favicon/favicon)

  (allow-reload-save-keybindings)

  (rdomc/render shimmer-root [page-root !active-route]))

;; initialize sketch on first-load
(defonce start-up (init))

;; TODO: support string for filename lookup but registry contains both relative
;; and absolute filenames.
(defn visit!
  "Force router to view a specific sketch in browser by keyword or namespace"
  [id]
  (when-let [sketch (cond (keyword? id)
                          (sketches/by-name id)
                          (symbol? id)
                          (sketches/by-ns id))]
    (view-sketch/sketch-link rfe/push-state (:sketch-id sketch))))

(comment (visit! 'shimmers.sketches.cube)
         (visit! :superposition)
         (visit! 'shimmers.sketches.unit-circle))
