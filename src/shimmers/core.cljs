(ns shimmers.core
  (:require [goog.dom :as dom]
            [quil.core :as q :include-macros true]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [reitit.coercion.spec :as rss]
            [reitit.frontend :as rf]
            [reitit.frontend.controllers :as rfc]
            [reitit.frontend.easy :as rfe]
            [shimmers.macros.loader :as loader :include-macros true]
            [shimmers.sketches.cube :as cube]
            [shimmers.sketches.dithering :as dithering]
            [shimmers.sketches.fire :as fire]
            ;; [shimmers.sketches.fluid :as fluid]
            [shimmers.sketches.hexaclock :as hexaclock]
            [shimmers.sketches.kd-tree :as kd-tree]
            [shimmers.sketches.langton-ant :as langton-ant]
            [shimmers.sketches.noise-grid :as noise-grid]
            [shimmers.sketches.particles :as particles]
            [shimmers.sketches.probabilistic-automata :as probabilistic-automata]
            [shimmers.sketches.random-walk :as random-walk]
            [shimmers.sketches.ray-marching :as ray-marching]
            [shimmers.sketches.ring :as ring]
            [shimmers.sketches.space-colonization :as space-colonization]
            [shimmers.sketches.zigzag :as zigzag]
            [shimmers.ui :as ui]))

(enable-console-print!)

;; detect window size for initial setup?
(defn fit-window []
  [(/ (.-innerWidth js/window) 2)
   (/ (.-innerHeight js/window) 2)])

(defn init-sketches [sketches]
  (atom {:sketches (into {} (for [sketch sketches] [(:id sketch) sketch]))
         :current nil}))

(defonce state
  (init-sketches
   (loader/sketches-with-meta
    :cube cube/run-sketch
    :dithering dithering/run-sketch
    :fire fire/run-sketch
    ;; :fluid fluid/run-sketch
    :hexaclock hexaclock/run-sketch
    :kd-tree kd-tree/run-sketch
    :langton-ant langton-ant/run-sketch
    :noise-grid noise-grid/run-sketch
    :ray-marching ray-marching/run-sketch
    :random-walk random-walk/run-sketch
    :ring ring/run-sketch
    :space-colonization space-colonization/run-sketch
    :particles particles/run-sketch
    :probabilistic-automata probabilistic-automata/run-sketch
    :zigzag zigzag/run-sketch)))

(defn run-current []
  (let [{:keys [sketches current]} @state
        sketch (get sketches current)]
    (when sketch
      (apply (:fn sketch) []))))

(defn stop-sketch []
  ;; force active video capture to stop
  (doseq [video (dom/getElementsByTagName "video")]
    (.stop (first (.getTracks (aget video "srcObject")))))
  ;; kill existing sketch
  (q/with-sketch (q/get-sketch-by-id "quil-host")
    (q/exit))
  (rdom/unmount-component-at-node (dom/getElement "explanation")))

(defn restart-sketch []
  (stop-sketch)
  (run-current))

(defn cycle-sketch []
  (let [{:keys [sketches current]} @state
        sketch-name (ui/cycle-next (keys sketches) current)]
    (rfe/push-state ::sketch-by-name {:name sketch-name})))

(defonce match (r/atom nil))

(defn sketch-list [params]
  (let [{:keys [sketches]} @state]
    [:section
     [:h1 "All Sketches"]
     (into [:ul]
           (for [[sketch _] sketches]
             [:li [:a {:href (rfe/href ::sketch-by-name {:name sketch})}
                   (name sketch)]]))]))

(defn sketch-by-name [params]
  (let [{:keys [sketches current]} @state]
    [:section {:class "controls"}
     [:span
      [:button {:on-click cycle-sketch} "Next"]
      [:button {:on-click restart-sketch} "Restart"]
      [:button {:on-click #(rfe/push-state ::sketch-list)} "All"]]
     [:span
      [:a {:href (:href (ui/code-link (get sketches current)))} (name current)]]
     [:span {:id "framerate"}]]))

(def routes
  [;; "/shimmers"
   ["/" ::root]
   ["/sketches" {:name ::sketch-list :view sketch-list}]
   ["/sketches/:name"
    {:name ::sketch-by-name
     :view sketch-by-name
     :parameters
     {:path {:name (every-pred string?
                               (set (map name (keys (get @state :sketches)))))}}
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
    (rfe/replace-state ::sketch-by-name {:name :particles})
    (swap! match
           (fn [old-match]
             (if new-match
               (assoc new-match :controllers
                      (rfc/apply-controllers (:controllers old-match) new-match)))))))

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


