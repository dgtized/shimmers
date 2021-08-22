(ns shimmers.sketches.iterative-displacement
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn vline [x]
  [(gv/vec2 x 0.1) (gv/vec2 x 0.9)])

(defn init-lines [n]
  (map vline (tm/norm-range (inc n))))

(defn displace-line [line lower upper]
  line)

(defn update-random-line
  [lines]
  (let [groups (partition 3 1 lines)
        k (rand-int (count groups))]
    (concat (take 1 lines)
            (map-indexed (fn [idx [lower line upper]]
                           (if (= idx k)
                             (displace-line line lower upper)
                             line)) groups)
            (take-last 1 lines))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:lines (init-lines 2)})

(defn update-state [state]
  (update state :lines update-random-line))

(defn draw [{:keys [lines]}]
  (q/no-fill)
  (doseq [points (butlast (rest lines))]
    (q/begin-shape)
    (doseq [[x y] (map cq/rel-pos points)]
      (q/vertex x y))
    (q/end-shape)))

(sketch/defquil iterative-displacement
  :created-at "2021-08-22"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
