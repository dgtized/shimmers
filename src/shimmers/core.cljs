(ns shimmers.core
  (:require
   [goog.dom :as dom]
   [reagent.core :as r]
   [reagent.dom.client :as rdomc]
   [reitit.coercion.spec :as rss]
   [reitit.frontend :as rf]
   [reitit.frontend.controllers :as rfc]
   [reitit.frontend.easy :as rfe]
   [shimmers.routing :as routing]
   [shimmers.sketches :as sketches]
   [shimmers.view.favicon :as favicon]
   [shimmers.view.sketch :as view-sketch]))

;; Uncomment to see javascript source of functions at repl
;; (set! cljs.core/*print-fn-bodies* true)
;; Or just (str the-function)

(defn on-navigate [page-match new-match]
  (if (or (nil? new-match) (= (:name (:data new-match)) :shimmers.routing/root))
    ;; default route, not sure on reitit for frontend routing
    (rfe/replace-state :shimmers.view.index/by-alphabetical)
    (swap! page-match
           (fn [old-match]
             (if new-match
               (assoc new-match :controllers
                      (rfc/apply-controllers (:controllers old-match) new-match))
               old-match)))))

(defonce shimmer-root (rdomc/create-root (dom/getElement "shimmer-mount")))
(defonce !active-route (r/atom nil))
(comment @!active-route)

(defn init []
  (rfe/start!
   ;; coercion here will cause missing sketches to explode
   (rf/router routing/routes {:data {:coercion rss/coercion}})
   (partial on-navigate !active-route)
   {:use-fragment true})

  ;; Render at least one frame of the favicon animation at start
  (favicon/favicon)

  (routing/allow-reload-save-keybindings)

  (rdomc/render shimmer-root [routing/page-root !active-route]))

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
