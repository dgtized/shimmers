(ns shimmers.sketches.flow-pairs
  (:require
   [clojure.edn :as edn]
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

(defonce ui-state (ctrl/state {:snap "0"}))

(defn make-pair [p]
  (let [distance (cq/rel-h (max 0.02 (dr/gaussian 0.075 0.05)))]
    {:p p
     :q (tm/+ p (dr/jitter distance))
     :distance distance
     :jitter (dr/gaussian 0.0 1.0)}))

(defn move-pos [pos elastic t dt]
  (let [open-space (tm/+ pos (gv/vec2 1000 1000))
        theta (* 4 eq/TAU (apply q/noise (tm/* (gv/vec3 open-space (* 4 t)) (* 0.5 dt))))
        snap (edn/read-string (:snap @ui-state))]
    (tm/+ (v/+polar pos (* (cq/rel-h 0.1) dt)
                    (if (> snap 0)
                      (tm/roundto theta (* (/ 1 snap) eq/TAU))
                      theta))
          (tm/* elastic dt))))

(defn move-pair [t dt]
  (fn [{:keys [p q resting-distance jitter] :as pair}]
    (let [elastic (tm/normalize (tm/- q p) (* 0.5 (- (g/dist p q) resting-distance)))]
      (-> pair
          (update :p move-pos elastic (+ t jitter) dt)
          (update :q move-pos (tm/- elastic) (+ t jitter 10.0) dt)))))

(defn in-bounds? [bounds]
  (fn [{:keys [p q]}]
    (or (g/contains-point? bounds p)
        (g/contains-point? bounds q))))

(defn update-pairs [pairs bounds t dt]
  (sequence
   (comp (filter (in-bounds? bounds))
         (map (move-pair t dt)))
   pairs))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/noise-seed (dr/random-int 10000))
  {:t 0.0
   :bounds (cq/screen-rect)
   :pairs (mapv make-pair (rp/random-points (cq/screen-rect 0.9) 128))})

(defn update-state [{:keys [t bounds] :as state}]
  (let [dt 0.01]
    (-> state
        (update :pairs update-pairs bounds t dt)
        (update :t + dt))))

(defn draw [{:keys [pairs]}]
  (q/stroke 0.0 0.06)
  (doseq [{:keys [p q]} pairs]
    (q/line p q))
  (when (< (count pairs) 64)
    (q/no-loop)))

(defn page []
  [sketch/with-explanation
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   (ctrl/container
    (ctrl/dropdown ui-state "Snap Resolution" [:snap]
                   {"Disabled" 0
                    "90 degrees" 4
                    "60 degrees" 6
                    "45 degrees" 8}))])

(sketch/definition flow-pairs
  {:created-at "2023-05-10"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
