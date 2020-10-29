(ns shimmers.core
  (:require [quil.core :as q :include-macros true]
            [goog.dom :as dom]
            [goog.events :as events]
            [shimmers.ray-marching :as ray-marching]
            [shimmers.particles-random-walk :as particles-random-walk]
            [shimmers.particles :as particles]))

(enable-console-print!)

;; detect window size for initial setup?
(defn fit-window []
  [(/ (.-innerWidth js/window) 2)
   (/ (.-innerHeight js/window) 2)])

;; initialize sketch on first-load

(defn ^:export run-sketch []
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

(def state (atom {:sketches {:points run-sketch
                             :ray-marching ray-marching/run-sketch
                             :random-walk particles-random-walk/run-sketch
                             :particles particles/run-sketch}}))

(defn run-current []
  (when-not (:current @state)
    (swap! state assoc :current :particles))
  (let [{:keys [sketches current]} (deref state)]
    (apply (get sketches current) [])
    ))

(defn cycle-sketch []
  (let [{:keys [sketches current]} (deref state)
        [sketch-name _] (find-next-sketch sketches current)]
    (swap! state assoc :current sketch-name)
    ;; kill existing sketch
    (q/with-sketch (q/get-sketch-by-id "quil-host")
      (q/exit))
    (run-current)))

(defn init []
  (events/listen (dom/getElement "next-sketch") "click"
                 (fn [] (cycle-sketch)))
  (run-current))

(defonce start-up (init))


