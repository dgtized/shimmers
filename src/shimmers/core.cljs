(ns shimmers.core
  (:require [quil.core :as q :include-macros true]
            [goog.dom :as dom]
            [goog.events :as events]
            [shimmers.framerate :as framerate]
            [shimmers.fluid :as fluid]
            [shimmers.noise-grid :as noise-grid]
            [shimmers.macros.loader :as loader :include-macros true]
            [shimmers.ray-marching :as ray-marching]
            [shimmers.particles-random-walk :as particles-random-walk]
            [shimmers.particles :as particles]
            [clojure.string :as str]))

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
    [(last (str/split file #"/"))
     (str (str/replace-first file #"^.*shimmers/src"
                             "https://github.com/dgtized/shimmers/blob/master/src")
          "#L"
          line)]
    ["" ""]))


(comment
  (code-link (loader/sketch-meta particles/run-sketch)))

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
    :fluid fluid/run-sketch
    :noise-grid noise-grid/run-sketch
    :ray-marching ray-marching/run-sketch
    :random-walk particles-random-walk/run-sketch
    :particles particles/run-sketch)
   (init-sketches :particles)))

;; TODO alternatively load from #url for direct linking?
(defn run-current []
  (let [{:keys [sketches current]} @state
        sketch (get sketches current)]
    (apply set-code-link (code-link sketch))
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


