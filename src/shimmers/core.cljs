(ns shimmers.core
  (:require [clojure.string :as str]
            [goog.dom :as dom]
            [goog.events :as events]
            [quil.core :as q :include-macros true]
            [reagent.dom :as rdom]
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

(defn set-code-link [text href]
  (let [link (dom/getElement "code-link")]
    (dom/setProperties link #js {"href" href})
    (dom/setTextContent link text)))

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
    (set-code-link (name (:id sketch)) (:href (code-link sketch)))
    (rdom/unmount-component-at-node (dom/getElement "explanation"))
    (apply (:fn sketch) [])
    ))

(defn restart-sketch []
  ;; force active video capture to stop
  (doseq [video (dom/getElementsByTagName "video")]
    (.stop (first (.getTracks (aget video "srcObject")))))
  ;; kill existing sketch
  (q/with-sketch (q/get-sketch-by-id "quil-host")
    (q/exit))
  (framerate/display "")
  (run-current))

(defn cycle-sketch []
  (let [{:keys [sketches current]} @state
        sketch-name (ui/cycle-next (keys sketches) current)]
    (swap! state assoc :current sketch-name)
    (ui/screen-view (name sketch-name))
    (restart-sketch)))

(defn init []
  ;; TODO consider generating elements at runtime
  ;; or change to reagent for top level function?
  (events/listen (dom/getElement "next-sketch") "click"
                 (fn [] (cycle-sketch)))
  (events/listen (dom/getElement "restart-sketch") "click"
                 (fn [] (restart-sketch)))
  (ui/screen-view (name (get @state :current)))
  (run-current))

;; initialize sketch on first-load
(defonce start-up (init))


