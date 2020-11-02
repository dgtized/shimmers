(ns shimmers.core
  (:require [quil.core :as q :include-macros true]
            [goog.dom :as dom]
            [goog.events :as events]
            [shimmers.framerate :as framerate]
            [shimmers.fluid :as fluid]
            [shimmers.noise-grid :as noise-grid]
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
  (if-let [{:keys [file line]} (meta sketch)]
    [(last (str/split file #"/"))
     (str (str/replace-first file #"^.*shimmers/src"
                             "https://github.com/dgtized/shimmers/blob/master/src")
          "#L"
          line)]
    ["" ""]))

(comment
  (code-link (var particles/run-sketch)))

(defn set-code-link [href text]
  (let [link (dom/getElement "code-link")]
    (dom/setProperties link #js {"href" href})
    (dom/setTextContent link text)))

(defn init-sketches [sketches default]
  (atom {:sketches sketches
         :current default}))

(defonce state
  (init-sketches {;; :test-sketch test-sketch
                  :fluid fluid/run-sketch
                  :noise-grid noise-grid/run-sketch
                  :ray-marching ray-marching/run-sketch
                  :random-walk particles-random-walk/run-sketch
                  :particles particles/run-sketch}
                 :particles))

;; TODO alternatively load from #url for direct linking?
(defn run-current []
  (let [{:keys [sketches current]} @state
        sketch-fn (get sketches current)]
    (set-code-link "" "")
    (apply sketch-fn [])
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


