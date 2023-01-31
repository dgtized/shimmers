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
   [thi.ng.geom.polygon :as gp]
   [thi.ng.math.core :as tm]))

(defn make-spot [pos max-radius slide]
  (let [r (dr/random 1.0 8.0)]
    {:pos pos
     :radius r
     :max-radius max-radius
     :slide slide
     :growth (max 1.0 (dr/gaussian 3.5 1.8))
     :crinkle (let [c (dr/pareto 0.025 1.05)]
                (tm/clamp (if (< c 0.032)
                            0
                            (+ c (dr/gaussian 0 0.05)))
                          0 1))
     :crinkle-wdith (dr/weighted {0 1
                                  0.25 1
                                  0.5 1
                                  0.66 2
                                  0.75 2})
     :points (vec (g/vertices (gc/circle r) (dr/random-int 20 80)))
     :spores (dr/chance 0.4)}))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:spots []
   :t 0
   :lifespan 100})

(defn similarity
  "Distance between two normalized vectors"
  [a b]
  (let [na (tm/normalize a)
        nb (tm/normalize b)]
    (* (g/dist na nb) 0.5)))

(defn update-spots [dt spots]
  (map (fn [{:keys [radius max-radius growth crinkle crinkle-width slide] :as spot}]
         (let [slow (- 1.0 (tm/smoothstep* 0.75 1.5 (/ radius max-radius)))
               dr (+ (* dt growth slow) (dr/gaussian 0.0 0.1))]
           (-> spot
               (update :pos (fn [pos] (tm/+ pos (tm/* slide dt))))
               (update :radius + dr)
               (update :points
                       (fn [points]
                         (mapv (fn [p]
                                 (let [factor (- 1.0 (similarity p slide))
                                       variance (* (tm/smoothstep* -0.1 crinkle-width factor)
                                                   crinkle)
                                       directional (dr/gaussian (+ dr (* 0.15 variance)) (* 0.3 variance))]
                                   (tm/+ p (tm/normalize p directional))))
                               points))))))
       spots))

(defn remove-dead [spots]
  (remove (fn [{:keys [radius max-radius]}]
            (> radius max-radius))
          spots))

(defn position-on-radius [spots]
  (let [candidates (remove (fn [{:keys [radius]}] (< radius (cq/rel-h 0.03))) spots)
        bounds (cq/screen-rect 0.9)
        inner (cq/screen-rect 0.8)]
    (->> (fn []
           (if-let [{:keys [pos radius]}
                    (and (dr/chance 0.66)
                         (seq candidates)
                         (dr/weighted-by
                          (fn [{:keys [radius max-radius]}]
                            (* max-radius (tm/smoothstep* 0.66 1.0 (/ radius max-radius))))
                          candidates))]
             (v/+polar pos (* (+ 1 (dr/gaussian 0.4 0.1)) radius) (dr/random eq/TAU))
             (let [{p :p [x y] :size} inner]
               (tm/+ p (dr/random x) (dr/random y)))))
         repeatedly
         (some (fn [p] (when (and (g/contains-point? bounds p)
                                 (not-any? (fn [{:keys [pos radius]}]
                                             (< (g/dist pos p) (* 0.9 radius)))
                                           spots))
                        p))))))

(defn add-spots [spots]
  (if (and (< (count spots) 64) (dr/chance 0.125))
    (conj spots
          (let [position (position-on-radius spots)
                max-radius
                (min (cq/rel-h (tm/clamp (+ (dr/pareto 0.01 1.3)
                                            (dr/gaussian 0.01 0.06))
                                         0.01 0.2))
                     (g/dist position (g/closest-point (cq/screen-rect 0.92) position)))]
            (make-spot position
                       max-radius
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
  (doseq [{:keys [pos radius max-radius points spores]} spots]
    (let [p-radius (/ radius max-radius)
          sqrt-r (Math/sqrt p-radius)]
      (q/stroke-weight (+ 0.5 (* 0.4 p-radius)))
      (cond (dr/chance 0.33)
            (do (q/no-fill)
                (q/stroke 0.0 (+ 0.1 (* 0.2 (tm/smoothstep* 0.4 1.0 p-radius))))
                (cq/draw-curve-shape (map (fn [p] (tm/+ pos p)) points)))
            (dr/chance 0.2)
            (do
              (q/stroke 0.0 (+ 0.1 (* 0.2 (tm/smoothstep* 0.4 1.0 p-radius))))
              (if (dr/chance (* p-radius 0.05))
                (q/fill 1.0 0.03)
                (q/no-fill))
              (cq/circle pos radius))
            :else
            (do (q/fill 0.0 0.1)
                (q/stroke 0.0 (+ 0.1 (* 0.15 (tm/smoothstep* 0.4 1.0 p-radius))))
                (let [polygon (gp/polygon2 points)]
                  (dotimes [_ (int (* 30 sqrt-r))]
                    (let [c (gc/circle (Math/abs (* sqrt-r (dr/gaussian (cq/rel-h 0.0025) 1.2))))
                          sample (g/point-at polygon (dr/random))
                          ext (if spores
                                (+ 0.94 (dr/pareto 0.05 tm/PHI))
                                1.0)]
                      (-> (if (dr/chance 0.5)
                            (triangle/inscribed-equilateral c (dr/random eq/TAU))
                            c)
                          (g/translate (tm/+ pos (tm/* sample ext)))
                          cq/draw-polygon))))))))
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
