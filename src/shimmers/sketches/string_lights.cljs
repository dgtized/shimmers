(ns shimmers.sketches.string-lights
  "Concept is bokah circles from out of focus string lights in a backdrop."
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.probability :as p]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.math.core :as tm]))

(defn string-line [p q n r]
  (for [point (repeatedly n #(tm/mix p q (rand)))]
    {:point (p/confusion-disk point r)
     :fill [1.0 (tm/random 0.6 0.9)]
     :size (rand-nth [8 9 10 11 12])}))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:strings
   (for [x (range 0.15 0.95 0.1)]
     (string-line (cq/rel-vec x 0.05) (cq/rel-vec (- x 0.05) 0.95)
                  (* 150 x) (* 20 x)))})

(defn update-state [state]
  state)

(defn draw [{:keys [strings]}]
  (q/background 0.15)
  (q/no-stroke)
  (doseq [string strings]
    (doseq [{:keys [point fill size]} string]
      (apply q/fill fill)
      (cq/circle point size))))

(sketch/defquil string-lights
  :created-at "2021-08-31"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
