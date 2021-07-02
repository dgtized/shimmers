(ns shimmers.core
  (:require [cljc.java-time.local-date :as ld]
            [clojure.set :as set]
            [clojure.string :as str]
            [goog.dom :as dom]
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
            [shimmers.view :as view]
            [spec-tools.data-spec :as ds]))

;; detect window size for initial setup?
(defn fit-window []
  [(/ (.-innerWidth js/window) 2)
   (/ (.-innerHeight js/window) 2)])

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

(defn restart-sketch [sketch]
  (view/sketch-link rfe/push-state (:id sketch)))

(defn cycle-sketch [sketch]
  (->> (name (:id sketch))
       (cs/cycle-next (sketches/known-names))
       (view/sketch-link rfe/push-state)))

;; FIXME: links are *always* fresh now since the seed is baked in
(defn sketch-list [sketches]
  (let [[sketches-an sketches-mz]
        (split-with (fn [{:keys [id]}] (re-find #"^[a-mA-M]" (name id)))
                    sketches)]
    [:section.sketch-list
     [:h1 (str "All Sketches (" (count sketches) ")")]
     [:p "A digital sketch-book of generative art, visual effects, computer
     animation, visualizations of algorithms, and whatever else struck my fancy to
     implement or explore. Many are complete, and some I periodically revisit
     and tweak. For those inspired by other's works or tutorials, I do my best
     to give attribution in the source code."]
     (view/selector :shimmers.view/sketch-list)
     [:div.sketch-columns
      [:div.column [:h3 "A-M"] (view/list-sketches sketches-an)]
      [:div.column [:h3 "N-Z"] (view/list-sketches sketches-mz)]]]))

(defn year-month [{:keys [created-at]}]
  [(ld/get-year created-at)
   (str/capitalize (str (ld/get-month created-at)))])

(defn sketches-by-date [sketches]
  (let [sketches-by-date (sort-by :created-at sketches)
        grouped-by-month (partition-by year-month sketches-by-date)]
    [:section.sketch-list
     (view/selector :shimmers.view/sketches-by-date)
     (for [sketches grouped-by-month
           :let [[year month] (year-month (first sketches))]]
       [:div {:key (str year month)}
        [:h3.date (str month " " year " (" (count sketches) ")")]
        (view/list-sketches sketches)])]))

(defn sketches-by-tag [sketches]
  (let [sketches (remove (fn [s] (empty? (:tags s))) sketches)
        tags (reduce (fn [acc {:keys [tags]}] (set/union acc tags))
                     #{}
                     sketches)]
    [:section.sketch-list
     (view/selector :shimmers.view/sketches-by-tag)
     (for [tag (sort-by name tags)
           :let [tagged-sketches (filter #(tag (:tags %)) sketches)]]
       [:div {:key (str tag)}
        [:h3.tag (str (str/capitalize (name tag))
                      " (" (count tagged-sketches) ")")]
        (view/list-sketches tagged-sketches)])]))

(defn sketch-by-name [{:keys [path]}]
  (let [sketch (sketches/by-name (:name path))]
    [:section.controls
     [:span
      [:button {:on-click #(cycle-sketch sketch)} "Next"]
      [:button {:on-click #(restart-sketch sketch)} "Restart"]
      [:button {:on-click #(rfe/push-state :shimmers.view/sketch-list)} "All"]]
     [:span
      [:a {:href (:href (ui/code-link sketch))} (name (:id sketch))]]
     [:span#framerate]]))

;; FIXME: handle invalid paths, re-route to sketch-list
(def routes
  [;; "/shimmers"
   ["/" ::root]
   ["/sketches" {:name :shimmers.view/sketch-list :view #(sketch-list (sketches/all))}]
   ["/sketches-by-date" {:name :shimmers.view/sketches-by-date :view #(sketches-by-date (sketches/all))}]
   ["/sketches-by-tag" {:name :shimmers.view/sketches-by-tag :view #(sketches-by-tag (sketches/all))}]
   ["/sketches/:name"
    {:name ::sketch-by-name
     :view sketch-by-name
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
