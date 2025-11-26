(ns shimmers.routing
  (:require
   [shimmers.sketches :as sketches]
   [shimmers.view.favicon :as favicon]
   [shimmers.view.index :as view-index]
   [shimmers.view.sketch :as view-sketch]
   [spec-tools.data-spec :as ds]))

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
