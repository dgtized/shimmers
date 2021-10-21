(ns shimmers.sketches.dreamcatcher
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.core :as sm]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce defo (debug/state))

(defn setup []
  (q/frame-rate 1.0)
  (q/color-mode :hsl 1.0)
  (let [shape (gc/circle (cq/rel-h 0.4))
        points (g/vertices shape 12)]
    {:shape shape
     :points (conj points (first points))}))

(defn between-pair? [last-angle]
  (fn [[a b]]
    (sm/radians-between? (g/heading b) (g/heading a) last-angle)))

(defn next-point [points]
  (let [[last-point & preceding] (reverse points)
        angle (g/heading last-point)
        pairs (drop 1 (partition 2 1 preceding))
        pair (first (filter (between-pair? angle) pairs))
        [a b] (or (seq pair) (last pairs))
        point (tm/mix (tm/mix a b 0.5) (gv/vec2) 0.2)]
    (reset! defo {:angle angle
                  :pairs (map (fn [p] [((between-pair? angle) p) (map g/heading p)]) pairs)
                  :pair (seq pair)
                  :last (last pairs)})
    (if (tm/delta= point last-point)
      a
      point)))

(defn update-state [{:keys [points] :as state}]
  (let [point (next-point points)]
    (if (not (tm/delta= point (last points)))
      (update state :points conj point)
      state)))

(defn draw [{:keys [points]}]
  (q/background 1.0)
  (q/no-fill)
  (q/with-translation (cq/rel-vec 0.5 0.5)
    (cq/draw-path points)))

(sketch/defquil dreamcatcher
  :created-at "2021-10-11"
  :on-mount (fn [] (debug/mount defo))
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
