(ns shimmers.sketches.scintillation
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]))

(defn setup []
  {:theta 0})

(defn update-state [state]
  (update state :theta + 0.005))

(defn draw [{:keys [theta]}]
  (q/background 255 32)
  (q/stroke-weight 0.5)
  (let [hspacing (+ 20 (* (q/cos theta) 10))
        hcount (/ (q/width) hspacing)
        vspacing (+ 20 (* (q/sin theta) 10))
        vcount (/ (q/height) vspacing)]
    (dotimes [i hcount]
      (q/line (* i hspacing) 0
              (+ 20 (* i 2 hspacing (q/cos theta))) (q/height)))
    (dotimes [j vcount]
      (q/line 0 (* j vspacing)
              (q/width) (+ 20 (* j 2 vspacing (q/sin theta)))))))

(defn ^:export run-sketch []
  (q/defsketch scintillation
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
