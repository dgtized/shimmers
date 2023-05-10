(ns shimmers.sketches.flow-pairs
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn make-pair [p]
  {:p p
   :q (tm/+ p (dr/jitter (cq/rel-h 0.025)))})

(defn move-pos [pos t dt]
  (let [open-space (tm/+ pos (gv/vec2 1000 1000))
        theta (* 2 tm/PHI eq/TAU (apply q/noise (tm/* (gv/vec3 open-space (* 2 t)) dt)))]
    (v/+polar pos (* (cq/rel-h 0.06) dt) theta)))

(defn move-pairs [pairs t dt]
  (mapv (fn [pair]
          (-> pair
              (update :p move-pos t dt)
              (update :q move-pos (+ t 5.0) dt)))
        pairs))

(defn cull-pairs [pairs bounds]
  (filter (fn [{:keys [p q]}]
            (or (g/contains-point? bounds p)
                (g/contains-point? bounds q)))
          pairs))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/noise-seed (dr/random-int 10000))
  {:t 0.0
   :bounds (cq/screen-rect)
   :pairs (mapv make-pair (rp/random-points (cq/screen-rect 0.9) 100))})

(defn update-state [{:keys [t bounds] :as state}]
  (let [dt 0.01]
    (-> state
        (update :pairs move-pairs t dt)
        (update :pairs cull-pairs bounds)
        (update :t + dt))))

(defn draw [{:keys [pairs]}]
  (q/stroke 0.0 0.05)
  (doseq [{:keys [p q]} pairs]
    (q/line p q))
  (when (empty? pairs)
    (q/no-loop)))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition flow-pairs
  {:created-at "2023-05-10"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
