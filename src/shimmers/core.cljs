(ns shimmers.core
  (:require
   [goog.dom :as dom]
   [reagent.core :as r]
   [reagent.dom.client :as rdomc]
   [reitit.frontend.easy :as rfe]
   [shimmers.routing :as routing]
   [shimmers.sketches :as sketches]
   [shimmers.view.sketch :as view-sketch]))

;; Uncomment to see javascript source of functions at repl
;; (set! cljs.core/*print-fn-bodies* true)
;; Or just (str the-function)

(defonce shimmer-root (rdomc/create-root (dom/getElement "shimmer-mount")))
(defonce !active-route (r/atom nil))
(comment @!active-route)

(defn init []
  (routing/start! !active-route)
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
