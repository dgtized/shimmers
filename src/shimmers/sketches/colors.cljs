(ns shimmers.sketches.colors
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [ui (atom {:lightness 50})
        applet (quil.sketch/current-applet)
        lightness (.createSlider applet 0 100 (:lightness @ui) 1)
        _ (.createSpan applet "Lightness")]
    (.changed lightness (fn [] (swap! ui assoc :lightness (.value lightness))))
    ui))

(defn update-state [state]
  state)

(defn draw [ui]
  (q/background 1.0)
  (q/no-stroke)
  (let [dx 0.01
        dy 0.01
        l (/ (:lightness @ui) 100)]
    (doseq [h (range 0 1 dx)]
      (doseq [s (range 0 1 dy)]
        (q/fill h s l 1.0)
        (q/rect (cq/rel-w h) (cq/rel-h s) (cq/rel-w dx) (cq/rel-h dy))))))

(defn ^:export run-sketch []
  (q/defsketch colors
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
