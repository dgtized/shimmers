(ns shimmers.sketches.circle-hatch
  "Test hatching approach, but for circles instead."
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.algorithm.line-clipping :as clip]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.sequence :as cs]
            [shimmers.math.probability :as p]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.circle :as gc]
            [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [circles (for [x (cs/centered-range 8)
                      y (cs/centered-range 6)]
                  (assoc (gc/circle (cq/rel-pos x y) (cq/rel-h (p/gaussian 0.05 0.01)))
                         :spacing (tm/random 2.5 10.0)
                         :theta (tm/random 0 tm/TWO_PI)))]
    {:circles circles}))

(defn update-state [{:keys [circles] :as state}]
  (let [k (rand-int (count circles))
        {:keys [spacing theta] :as circle} (nth circles k)
        hatches (clip/hatch-circle circle spacing theta)]
    (-> state
        (assoc :circles (map-indexed (fn [i c] (if (= i k)
                                                (assoc c
                                                       :theta (+ theta (* 0.2 (p/happensity 0.1)))
                                                       :spacing (tm/random 2.5 10.0))
                                                c)) circles))
        (assoc-in [:hatches k] hatches))))

(defn draw [{:keys [circles hatches]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
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
