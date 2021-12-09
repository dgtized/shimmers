(ns shimmers.sketches.spaces-divided
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.math.core :as tm]))

(defn gen-line [bounds]
  (fn []
    (let [[[p q] & rest] (dr/shuffle (g/edges bounds))
          [a b] (dr/rand-nth rest)]
      (gl/line2 (tm/mix p q (dr/random 0.1 0.9))
                (tm/mix a b (dr/random 0.1 0.9))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect)]
    {:lines (repeatedly 8 (gen-line bounds))}))

(defn update-state [{:keys [lines] :as state}]
  state)

(defn draw [{:keys [lines]}]
  (doseq [{[p q] :points} lines]
    (q/line p q)))

(sketch/defquil spaces-divided
  :created-at "2021-12-09"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
