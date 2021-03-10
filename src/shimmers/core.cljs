(ns shimmers.core
  (:require [goog.dom :as dom]
            [quil.core :as q :include-macros true]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [reitit.coercion.spec :as rss]
            [reitit.frontend :as rf]
            [reitit.frontend.controllers :as rfc]
            [reitit.frontend.easy :as rfe]
            [shimmers.common.ui :as ui]
            [shimmers.macros.loader :as loader :include-macros true]
            [shimmers.sketches.ascendance :as ascendance]
            [shimmers.sketches.braid :as braid]
            [shimmers.sketches.bubbles :as bubbles]
            [shimmers.sketches.butterfly :as butterfly]
            [shimmers.sketches.cube :as cube]
            [shimmers.sketches.dithering :as dithering]
            [shimmers.sketches.emitters :as emitters]
            [shimmers.sketches.fire :as fire]
            [shimmers.sketches.folding-triangles :as folding-triangles]
            ;; [shimmers.sketches.fluid :as fluid]
            [shimmers.sketches.gravity-well :as gravity-well]
            [shimmers.sketches.hexaclock :as hexaclock]
            [shimmers.sketches.k-means :as k-means]
            [shimmers.sketches.kd-tree :as kd-tree]
            [shimmers.sketches.langton-ant :as langton-ant]
            [shimmers.sketches.noise-grid :as noise-grid]
            [shimmers.sketches.noisy-shapes :as noisy-shapes]
            [shimmers.sketches.object-permanence :as object-permanence]
            [shimmers.sketches.particles :as particles]
            [shimmers.sketches.permutations-of-transfiguration :as permutations-of-transfiguration]
            [shimmers.sketches.precipitation :as precipitation]
            [shimmers.sketches.probabilistic-automata :as probabilistic-automata]
            [shimmers.sketches.radar :as radar]
            [shimmers.sketches.random-walk :as random-walk]
            [shimmers.sketches.ray-marching :as ray-marching]
            [shimmers.sketches.ring :as ring]
            [shimmers.sketches.ripples :as ripples]
            [shimmers.sketches.rose :as rose]
            [shimmers.sketches.scintillation :as scintillation]
            [shimmers.sketches.scribbles :as scribbles]
            [shimmers.sketches.space-colonization :as space-colonization]
            [shimmers.sketches.sphere :as sphere]
            [shimmers.sketches.substrate :as substrate]
            [shimmers.sketches.superposition :as superposition]
            [shimmers.sketches.triangulating-subdivisions :as triangulating-subdivisions]
            [shimmers.sketches.tunnel-flight :as tunnel-flight]
            [shimmers.sketches.typography :as typography]
            [shimmers.sketches.video-shader :as video-shader]
            [shimmers.sketches.yin-yang :as yin-yang]
            [shimmers.sketches.zigzag :as zigzag]))

(enable-console-print!)

;; detect window size for initial setup?
(defn fit-window []
  [(/ (.-innerWidth js/window) 2)
   (/ (.-innerHeight js/window) 2)])

(defn init-sketches [sketches]
  (atom {:sketches (sort-by (comp name :id) sketches)
         :current nil}))

(defonce state
  (init-sketches
   (loader/sketches-with-meta
    [ascendance/run-sketch
     braid/run-sketch
     bubbles/run-sketch
     butterfly/run-sketch
     cube/run-sketch
     dithering/run-sketch
     fire/run-sketch
     ;; folding-triangles/run-sketch
     ;; fluid/run-sketch
     gravity-well/run-sketch
     hexaclock/run-sketch
     emitters/run-sketch
     k-means/run-sketch
     kd-tree/run-sketch
     langton-ant/run-sketch
     noise-grid/run-sketch
     noisy-shapes/run-sketch
     object-permanence/run-sketch
     particles/run-sketch
     permutations-of-transfiguration/run-sketch
     precipitation/run-sketch
     probabilistic-automata/run-sketch
     radar/run-sketch
     random-walk/run-sketch
     ray-marching/run-sketch
     ring/run-sketch
     ripples/run-sketch
     rose/run-sketch
     scintillation/run-sketch
     scribbles/run-sketch
     space-colonization/run-sketch
     sphere/run-sketch
     substrate/run-sketch
     superposition/run-sketch
     triangulating-subdivisions/run-sketch
     tunnel-flight/run-sketch
     typography/run-sketch
     video-shader/run-sketch
     yin-yang/run-sketch
     zigzag/run-sketch])))

(defn current-sketch []
  (let [{:keys [sketches current]} @state]
    (first (filter #(= current (:id %)) sketches))))

(defn run-current []
  (when-let [sketch (current-sketch)]
    (apply (:fn sketch) [])))

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
        next-sketch (ui/cycle-next (map :id sketches) current)]
    (rfe/push-state ::sketch-by-name {:name next-sketch})))

(defonce match (r/atom nil))

(defn sketch-list []
  (let [{:keys [sketches]} @state]
    [:section
     [:h1 "All Sketches"]
     (into [:ul]
           (for [sketch sketches]
             [:li [:a {:href (rfe/href ::sketch-by-name {:name (:id sketch)})}
                   (:id sketch)]]))]))

(defn sketch-by-name []
  (let [active (current-sketch)]
    [:section {:class "controls"}
     [:span
      [:button {:on-click cycle-sketch} "Next"]
      [:button {:on-click restart-sketch} "Restart"]
      [:button {:on-click #(rfe/push-state ::sketch-list)} "All"]]
     [:span
      [:a {:href (:href (ui/code-link active))} (name (:id active))]]
     [:span {:id "framerate"}]]))

(defn known-sketches []
  (map (comp name :id) (get @state :sketches)))

(def routes
  [;; "/shimmers"
   ["/" ::root]
   ["/sketches" {:name ::sketch-list :view sketch-list}]
   ["/sketches/:name"
    {:name ::sketch-by-name
     :view sketch-by-name
     :parameters
     {:path {:name (every-pred string? (set (known-sketches)))}}
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


