(ns shimmers.sketches.s-charatarum
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(defn make-spot [pos max-radius growth slide]
  {:pos pos
   :radius 0.01
   :max-radius max-radius
   :growth growth
   :slide slide})

(defn setup []
  (q/color-mode :hsl 1.0)
  {:spots []
   :t 0
   :lifespan 100})

(defn update-spots [dt spots]
  (map (fn [{:keys [max-radius growth slide] :as spot}]
         (-> spot
             (update :pos (fn [pos] (tm/+ pos (tm/* slide dt))))
             (update :radius
                     (fn [radius]
                       (if (< radius max-radius)
                         (let [slow (- 1.0 (tm/smoothstep* 0.75 1.5 (/ radius max-radius)))]
                           (+ radius (* dt growth slow) (dr/gaussian 0.0 0.2)))
                         radius)))))
       spots))

(defn remove-dead [spots]
  (remove (fn [{:keys [radius max-radius]}]
            (> radius max-radius))
          spots))

(defn add-spots [spots]
  (if (and (< (count spots) 64) (dr/chance 0.12))
    (conj spots
          (let [max-radius (cq/rel-h (tm/clamp (+ (dr/pareto 0.02 1.25)
                                                  (dr/gaussian 0.0 0.05))
                                               0.01 0.3))]
            (make-spot (cq/rel-vec (dr/random) (dr/random))
                       max-radius
                       (dr/random 2.0 5.0)
                       (dr/randvec2 (/ (cq/rel-h 0.05) max-radius)))))
    spots))

(defn update-state [{:keys [t lifespan] :as state}]
  (let [dt (dr/random 0.05 0.20)]
    (if (< t lifespan)
      (-> state
          (update :t + dt)
          (update :spots (comp
                          remove-dead
                          add-spots
                          (partial update-spots dt))))
      state)))

(defn draw [{:keys [lifespan spots t]}]
  (q/ellipse-mode :radius)
  (q/stroke-weight 0.5)
  (doseq [{:keys [pos radius max-radius]} spots]
    (let [p-radius (/ radius max-radius)
          sqrt-r (Math/sqrt p-radius)]
      (if (dr/chance 0.5)
        (do
          (q/stroke 0.0 (+ 0.15 (* 0.4 (tm/smoothstep* 0.4 1.0 p-radius))))
          (if (dr/chance (* p-radius 0.33))
            (q/fill 1.0 0.01)
            (q/no-fill))
          (cq/circle pos radius))
        (do (q/fill 0.0 0.1)
            (q/stroke 0.0 (+ 0.1 (* 0.2 (tm/smoothstep* 0.4 1.0 p-radius))))
            (dotimes [_ (dr/random-int (int (* 48 sqrt-r)))]
              (-> (gc/circle (Math/abs (* sqrt-r (dr/gaussian (cq/rel-h 0.005) 1.0))))
                  (triangle/inscribed-equilateral (dr/random eq/TAU))
                  (g/translate (v/+polar pos radius (dr/random eq/TAU)))
                  cq/draw-polygon))))))
  (when (> t lifespan)
    (q/no-loop)))

(sketch/defquil s-charatarum
  :created-at "2023-01-30"
  :tags #{}
  :size [900 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
