(ns shimmers.sketches.memory-allocation
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]))

;; 4k blocks 2**24
;; 4k bytes per block 2**12

(defn setup []
  (q/color-mode :hsl 1.0)
  {:size (Math/pow 2 12)
   :pages []})

(defn update-state [state]
  state)

(defn draw [{:keys [size]}]
  (q/background 1.0)
  (q/no-fill)
  (let [aspect (/ (q/width) (q/height))
        cols (int (Math/sqrt size))
        w (/ (/ (q/width) cols) aspect)
        h (/ (q/height) (/ size cols))]
    (println cols)
    (doseq [y (range 0 (/ size cols))]
      (doseq [x (range 0 cols 1)]
        (q/rect (* x w) (* y h) w h)))))

(sketch/defquil memory-allocation
  :created-at "2021-10-14"
  :size [800 800]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
