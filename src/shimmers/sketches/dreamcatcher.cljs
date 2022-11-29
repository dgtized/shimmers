(ns shimmers.sketches.dreamcatcher
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.debug :as debug]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce defo (debug/state))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [shape (gc/circle (cq/rel-h 0.48))
        points (g/vertices shape 16)]
    {:shape shape
     :points points
     :row points}))

(defn next-row [row]
  (map (fn [[a b]] [(tm/mix (tm/mix a b 0.5) (gv/vec2) 0.1) b])
       (partition 2 1 (conj row (first row)))))

(defn update-state [{:keys [points row] :as state}]
  (if (< (count points) 1000)
    (let [added-row (next-row row)]
      (-> state
          (update :points concat
                  (cons (last (last added-row)) (mapcat identity added-row)))
          (assoc :row (mapv first added-row))))
    state)
  )

(defn draw [{:keys [points row]}]
  (q/background 1.0)
  (q/no-fill)
  (q/with-translation (cq/rel-vec 0.5 0.5)
    (cq/draw-path points)))

(sketch/defquil dreamcatcher
  :created-at "2021-10-11"
  :on-mount (debug/mount defo)
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
