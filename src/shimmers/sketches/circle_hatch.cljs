(ns shimmers.sketches.circle-hatch
  "Test hatching approach, but for circles instead."
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.circle :as gc]))

(defn centered-range [n]
  (let [elements (inc n)]
    (->> (rest (range elements))
         (mapv #(/ % (double elements))))))

(comment (map (fn [x] [x (centered-range x)]) (range 6)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:circles (for [x (centered-range 4)
                  y (centered-range 4)]
              (gc/circle (cq/rel-pos x y) (cq/rel-w 0.1)))})

(defn update-state [state]
  state)

(defn draw [{:keys [circles]}]
  (q/background 1.0)
  (q/no-fill)
  (doseq [{:keys [p r]} circles]
    (cq/circle p r)))

(sketch/defquil circle-hatch
  :created-at "2021-09-02"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
