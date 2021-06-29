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
            [shimmers.math.deterministic-random :as dr]
            [shimmers.sketches :as sketches]
            [cljc.java-time.local-date :as ld]
            [spec-tools.data-spec :as ds]))

;; detect window size for initial setup?
(defn fit-window []
  [(/ (.-innerWidth js/window) 2)
   (/ (.-innerHeight js/window) 2)])

(defn generate-seed []
  (rand-int (Math/pow 2 32)))

(defn known-sketches []
  (map (comp name :id) (sketches/all)))

(defn start-sketch [sketch]
  ;; TODO wire up :seed to pass to run-sketch

  ;; unfortunately neither Clojurescript or Javascript have an interface for
  ;; setting a seed. For quil based sketches, a seed can be specified as that is
  ;; encoded into the p5js applet state. However that will not inform
  ;; rand-nth/rand-int/rand or thi.ng/math/random calls. So need to handle that
  ;; on a sketch by sketch basis and find or implement a library to help.
  (when-let [seed (:seed sketch)]
    ;; migrates set random-seed to sketches that use it?
    ;; performance optimizations?
    (dr/random-seed seed))

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

;; Note that seed is required so that the path "changes", even though sketches
;; are not using seed. Related to this, cycle and links from the sketch-list are
;; *not* including a seed, so that will need to be included by default somehow?
(defn restart-sketch [sketch]
  (rfe/push-state ::sketch-by-name
                  {:name (:id sketch)}
                  {:seed (generate-seed)}))

(defn cycle-sketch [sketch]
  (let [next-sketch (cs/cycle-next (known-sketches) (name (:id sketch)))]
    (rfe/push-state ::sketch-by-name
                    {:name next-sketch}
                    {:seed (generate-seed)})))

(defn list-sketches [sketches]
  (into [:ul]
        (for [sketch sketches]
          [:li [:a {:href (rfe/href ::sketch-by-name
                                    {:name (:id sketch)}
                                    {:seed (generate-seed)})
                    :title (if-let [created-at (:created-at sketch)]
                             (str created-at)
                             "")}
                (:id sketch)]])))

;; FIXME: links are *always* fresh now since the seed is baked in
(defn sketch-list []
  (let [sketches (sketches/all)
        [sketches-an sketches-mz]
        (split-with (fn [{:keys [id]}] (re-find #"^[a-mA-M]" (name id)))
                    sketches)]
    [:section {:class "sketch-list"}
     [:h1 (str "All Sketches (" (count sketches) ")")]
     [:p "A digital sketch-book of generative art, visual effects, computer
     animation, visualizations of algorithms, and whatever else struck my fancy to
     implement or explore. Many are complete, and some I periodically revisit
     and tweak. For those inspired by other's works or tutorials, I do my best
     to give attribution in the source code."]
     [:div {:class "sketch-columns"}
      [:div [:h3 "A-M"] (list-sketches sketches-an)]
      [:div [:h3 "N-Z"] (list-sketches sketches-mz)]]]))

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
