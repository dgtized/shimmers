(ns shimmers.sketches.circle-hatch
  "Test hatching approach, but for circles instead."
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.circle :as gc]
            [thi.ng.math.core :as tm]
            [shimmers.algorithm.line-clipping :as clip]))

(defn centered-range [n]
  (let [elements (inc n)]
    (->> (rest (range elements))
         (mapv #(/ % (double elements))))))

(comment (map (fn [x] [x (centered-range x)]) (range 6)))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [circles (for [x (centered-range 4)
                      y (centered-range 4)]
                  (assoc (gc/circle (cq/rel-pos x y) (cq/rel-w 0.1))
                         :spacing (tm/random 2.5 10.0)
                         :theta (tm/random 0 tm/TWO_PI)))]
    {:circles circles}))

(defn update-state [{:keys [circles] :as state}]
  (let [{:keys [spacing theta] :as circle} (rand-nth circles)]
    (assoc-in state [:hatches circle] (clip/hatch-circle circle spacing theta))))

(defn draw [{:keys [circles hatches]}]
  (q/background 1.0)
  (q/no-fill)
  (doseq [{:keys [p r]} circles]
    (cq/circle p r))
  (doseq [hatching (vals hatches)]
    (doseq [{[p q] :points} hatching]
      (q/line p q))))

(sketch/defquil circle-hatch
  :created-at "2021-09-02"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
