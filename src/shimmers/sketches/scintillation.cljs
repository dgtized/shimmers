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
      (let [x0 (* i hspacing)
            x1 (+ 20 (* i 2 hspacing (q/cos theta)))]
        (q/line x0 0 x1 (q/height))
        (q/line x1 0 x0 (q/height))))
    (dotimes [j vcount]
      (let [y0 (* j vspacing)
            y1 (+ 20 (* j 2 vspacing (q/sin theta)))]
        (q/line 0 y0 (q/width) y1)
        (q/line 0 y1 (q/width) y0)))))

(defn ^:export run-sketch []
  (q/defsketch scintillation
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
