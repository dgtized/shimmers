(ns shimmers.core
  (:require [clojure.string :as str]
            [goog.dom :as dom]
            [goog.events :as events]
            [quil.core :as q :include-macros true]
            [shimmers.cube :as cube]
            [shimmers.framerate :as framerate]
            [shimmers.fire :as fire]
            [shimmers.fluid :as fluid]
            [shimmers.macros.loader :as loader :include-macros true]
            [shimmers.noise-grid :as noise-grid]
            [shimmers.particles :as particles]
            [shimmers.random-walk :as random-walk]
            [shimmers.ray-marching :as ray-marching]))

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

(defn find-next-sketch [sketches current]
  (->> sketches
       cycle
       (drop-while (fn [x] (not (= current (first x)))))
       (drop 1)
       first))

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
    :fire fire/run-sketch
    ;; :fluid fluid/run-sketch
    :noise-grid noise-grid/run-sketch
    :ray-marching ray-marching/run-sketch
    :random-walk random-walk/run-sketch
    :particles particles/run-sketch)
   (init-sketches :cube)))

;; TODO alternatively load from #url for direct linking?
(defn run-current []
  (let [{:keys [sketches current]} @state
        sketch (get sketches current)]
    (set-code-link (name (:id sketch)) (:href (code-link sketch)))
    (apply (:fn sketch) [])
    ))

(defn restart-sketch []
  ;; kill existing sketch
  (q/with-sketch (q/get-sketch-by-id "quil-host")
    (q/exit))
  (framerate/display "")
  (run-current))

(defn cycle-sketch []
  (let [{:keys [sketches current]} @state
        [sketch-name _] (find-next-sketch sketches current)]
    (swap! state assoc :current sketch-name)
    (restart-sketch)))

(defn init []
  ;; TODO consider generating elements at runtime
  ;; or change to reagent for top level function?
  (events/listen (dom/getElement "next-sketch") "click"
                 (fn [] (cycle-sketch)))
  (events/listen (dom/getElement "restart-sketch") "click"
                 (fn [] (restart-sketch)))
  (run-current))

;; initialize sketch on first-load
(defonce start-up (init))


