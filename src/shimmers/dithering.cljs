(ns shimmers.dithering
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn setup []
  (let [applet (quil.sketch/current-applet)
        capture (.createCapture applet "video")]
    (.size capture 640 480)
    (.hide capture)
    {:capture capture}))

(defn update-state [state]
  state)

(defn draw [{:keys [capture]}]
  (q/background "white")
  (q/image capture 0 0 640 480))

(defn ^:export run-sketch []
  (q/defsketch shimmers
    :host "quil-host"
    :size [640 480]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode]))
