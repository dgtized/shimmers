(ns shimmers.sketches.s-chartarum
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
  (map (fn [{:keys [radius max-radius growth slide] :as spot}]
         (let [slow (- 1.0 (tm/smoothstep* 0.75 1.5 (/ radius max-radius)))
               dr (+ (* dt growth slow) (dr/gaussian 0.0 0.2))]
           (-> spot
               (update :pos (fn [pos] (tm/+ pos (tm/* slide dt))))
               (update :radius + dr))))
       spots))

(defn remove-dead [spots]
  (remove (fn [{:keys [radius max-radius]}]
            (> radius max-radius))
          spots))

(defn position-on-radius [spots]
  (let [candidates (remove (fn [{:keys [radius]}] (< radius (cq/rel-h 0.03))) spots)
        bounds (cq/screen-rect 0.9)]
    (->> (fn []
           (if-let [{:keys [pos radius]}
                    (and (dr/chance 0.4)
                         (seq candidates)
                         (dr/weighted-by
                          (fn [{:keys [radius max-radius]}]
                            (tm/smoothstep* 0.66 1.0 (/ radius max-radius)))
                          candidates))]
             (v/+polar pos (* 1.05 radius) (dr/random eq/TAU))
             (let [{p :p [x y] :size} bounds]
               (tm/+ p (dr/random x) (dr/random y)))))
         repeatedly
         (some (fn [p] (when (and (g/contains-point? bounds p)
                                 (not-any? (fn [{:keys [pos radius]}]
                                             (< (g/dist pos p) (* 0.9 radius)))
                                           spots))
                        p))))))

(defn add-spots [spots]
  (if (and (< (count spots) 64) (dr/chance 0.12))
    (conj spots
          (let [position (position-on-radius spots)
                max-radius
                (min (cq/rel-h (tm/clamp (+ (dr/pareto 0.01 1.3)
                                            (dr/gaussian 0.01 0.06))
                                         0.01 0.2))
                     (g/dist position (g/closest-point (cq/screen-rect 0.92) position)))]
            (make-spot position
                       max-radius
                       (max 1.0 (dr/gaussian 3.0 1.0))
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
  (doseq [{:keys [pos radius max-radius]} spots]
    (let [p-radius (/ radius max-radius)
          sqrt-r (Math/sqrt p-radius)]
      (q/stroke-weight (+ 0.5 (* 0.4 p-radius)))
      (if (dr/chance 0.5)
        (do
          (q/stroke 0.0 (+ 0.15 (* 0.4 (tm/smoothstep* 0.4 1.0 p-radius))))
          (if (dr/chance (* p-radius 0.1))
            (q/fill 1.0 0.03)
            (q/no-fill))
          (cq/circle pos radius))
        (do (q/fill 0.0 0.1)
            (q/stroke 0.0 (+ 0.1 (* 0.2 (tm/smoothstep* 0.4 1.0 p-radius))))
            (dotimes [_ (int (* 28 sqrt-r))]
              (-> (gc/circle (Math/abs (* sqrt-r (dr/gaussian (cq/rel-h 0.005) 1.0))))
                  (triangle/inscribed-equilateral (dr/random eq/TAU))
                  (g/translate (v/+polar pos radius (dr/random eq/TAU)))
                  cq/draw-polygon))))))
  (when (> t lifespan)
    (q/no-loop)))

(sketch/defquil s-chartarum
  :created-at "2023-01-30"
  :tags #{:deterministic}
  :size [900 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
