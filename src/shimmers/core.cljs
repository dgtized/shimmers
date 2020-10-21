(ns shimmers.core
  (:require [quil.core :as q :include-macros true]
            [shimmers.ray-marching :as ray-marching]))

(enable-console-print!)

;; initialize sketch on first-load

(defn ^:export run-sketch []
  (q/defsketch points
    :host "quil-host"
    :size [500 500]
    :setup (fn [] (q/background "white"))
    :draw (fn [] (q/point (q/random (q/width))
                         (q/random (q/height))))))

(defonce start-up
  (run-sketch)
  #_(ray-marching/run-sketch)
  )


