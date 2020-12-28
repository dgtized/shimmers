(ns shimmers.core
  (:require [clojure.string :as str]
            [goog.dom :as dom]
            [goog.events :as events]
            [quil.core :as q :include-macros true]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [reitit.frontend :as rf]
            [reitit.frontend.easy :as rfe]
            [reitit.frontend.controllers :as rfc]
            [shimmers.framerate :as framerate]
            [shimmers.macros.loader :as loader :include-macros true]
            [shimmers.sketches.cube :as cube]
            [shimmers.sketches.dithering :as dithering]
            [shimmers.sketches.fire :as fire]
            [shimmers.sketches.fluid :as fluid]
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

(defn ^:export test-sketch []
  (q/defsketch points
    :host "quil-host"
    :size [500 500]
    :setup (fn [] (q/background "white"))
    :draw (fn [] (q/point (q/random (q/width))
                         (q/random (q/height))))))

(defn code-link [sketch]
  (if-let [{:keys [file line]} (:meta sketch)]
    {:filename (last (str/split file #"/"))
     :href
     (-> file
         (str/replace-first #"^.*shimmers/src"
                            "https://github.com/dgtized/shimmers/blob/master/src")
         (str "#L" line))}
    {:filename "" :href ""}))

(comment
  (code-link (first (loader/sketches-with-meta :particles particles/run-sketch))))

(defn init-sketches [sketches default]
  (atom {:sketches (into {} (for [sketch sketches] [(:id sketch) sketch]))
         :current default}))

(defonce state
  (->
   (loader/sketches-with-meta
    ;; :test-sketch test-sketch
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
    :zigzag zigzag/run-sketch)
   (init-sketches :particles)))

;; TODO alternatively load from #url for direct linking?
(defn run-current []
  (let [{:keys [sketches current]} @state
        sketch (get sketches current)]
    (rdom/unmount-component-at-node (dom/getElement "explanation"))
    (apply (:fn sketch) [])
    ))

(defn stop-sketch []
  ;; force active video capture to stop
  (doseq [video (dom/getElementsByTagName "video")]
    (.stop (first (.getTracks (aget video "srcObject")))))
  ;; kill existing sketch
  (q/with-sketch (q/get-sketch-by-id "quil-host")
    (q/exit))
  (framerate/display ""))

(defn restart-sketch []
  (stop-sketch)
  (run-current))

(defn cycle-sketch []
  (let [{:keys [sketches current]} @state
        sketch-name (ui/cycle-next (keys sketches) current)]
    (rfe/push-state ::sketch-by-name {:name sketch-name})))

(defonce match (r/atom nil))

(defn logging-controller [name & opts]
  (let [default {:start (println "start controller" name)
                 :stop (println "stop controller" name)}]
    (if opts
      (merge default opts)
      default)))

(defn sketch-list [params]
  (let [{:keys [sketches]} @state]
    [:div [:h1 "Sketch List"]
     (into [:ul]
           (for [[sketch _] sketches]
             [:li [:a {:href (rfe/href ::sketch-by-name {:name sketch})}
                   (name sketch)]]))]))

(defn sketch-by-name [params]
  (let [{:keys [sketches current]} @state]
    [:section {:class "controls"}
     [:span
      [:a {:href (:href (code-link (get sketches current)))} (name current)]]
     [:span
      [:button {:on-click cycle-sketch} "Next"]
      [:button {:on-click restart-sketch} "Restart"]
      [:button {:on-click #(rfe/push-state ::sketch-list)} "All"]]
     [:span {:id "framerate"}]]))

(def routes
  ["/"
   ["" ::root]
   ["sketches"
    [""
     {:name ::sketch-list
      :view sketch-list
      :controllers [(logging-controller "sketch list")]}]
    ["/:name"
     {:name ::sketch-by-name
      :view sketch-by-name
      :parameters {:path {:name keyword?}}
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
                (stop-sketch))}]}]]])

(defn on-navigate [new-match]
  (swap! match
         (fn [old-match]
           (println "router: " old-match "->" new-match)
           (if new-match
             (assoc new-match :controllers
                    (rfc/apply-controllers (:controllers old-match) new-match))))))

(defn page-root []
  (let [page @match
        view (:view (:data page))]
    (when view
      [view (:parameters page)])))

(defn init []
  (rfe/start!
   (rf/router routes)
   on-navigate
   {:use-fragment false})

  (rdom/render [page-root] (dom/getElement "list-sketches")))

;; initialize sketch on first-load
(defonce start-up (init))


