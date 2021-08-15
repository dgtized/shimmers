(ns shimmers.sketches.concentric-moire
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]
            [shimmers.common.quil :as cq]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:circles [{:pos (cq/rel-pos 0.2 0.5)
              :spacing 12 :weight 1.2}
             {:pos (cq/rel-pos 0.8 0.5)
              :spacing 12 :weight 1.2}
             {:pos (cq/rel-pos 0.5 0.25)
              :spacing 4 :weight 0.5}
             {:pos (cq/rel-pos 0.5 0.75)
              :spacing 4 :weight 0.5}]})

(defn update-state [state]
  state)

(defn draw [{:keys [circles]}]
  (q/background 1.0)
  (q/no-fill)
  (q/ellipse-mode :radius)
  (doseq [{:keys [pos spacing weight]} circles]
    (q/stroke-weight weight)
    (doseq [r (range 2 96)]
      (cq/circle pos (* spacing r)))))

(sketch/defquil concentric-moire
  :created-at "2021-08-14"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
