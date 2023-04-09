(ns shimmers.sketches.liminal-tension
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.math.control :as control]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.math.core :as tm]))

(defrecord Particle [color pos angle vel angle-vel dest scale wiggle decay])

(defn perp-motion [pos dest wiggle t]
  (let [v (tm/- pos dest)
        wobble (* wiggle
                  (/ (tm/mag v) (inc t))
                  (Math/sin t))]
    (g/rotate (tm/normalize v wobble) (* 0.25 eq/TAU))))

(defn move [dt t pos-c angle-c drag]
  (fn [{:keys [pos angle vel angle-vel dest wiggle decay] :as particle}]
    (let [force (tm/+ (control/force-accel pos dest pos-c vel)
                      (perp-motion pos dest wiggle t))
          angle-target (g/heading (tm/- dest pos))
          angle-acc (control/angular-acceleration angle angle-target angle-c angle-vel)
          drag-c (- 1.0 (eq/sqr (* drag dt)))
          particle' (-> particle
                        (assoc
                         :pos (tm/+ pos (tm/* vel dt))
                         :angle (+ angle (* angle-vel dt))
                         :vel (tm/limit (tm/* (tm/+ vel (tm/* force dt)) drag-c)
                                        (+ (* 3 pos-c) (/ 1.0 dt)))
                         :angle-vel (* (+ angle-vel (* angle-acc dt)) drag-c)))]
      (cond (> (g/dist pos dest) 0.1)
            particle'
            (> decay 0.0)
            (update particle' :decay - (dr/random dt))
            :else
            nil))))

(defn gen-particle [line boundary color]
  (fn []
    (let [pos (g/point-at line (dr/random))]
      (map->Particle
       {:color (if (dr/chance 0.05) color [0.0 0.0 0.0])
        :pos pos
        :angle (dr/random-tau)
        :vel (dr/randvec2 20)
        :angle-vel (dr/gaussian 0.0 2.0)
        :dest (tm/mix pos
                      (g/closest-point boundary pos)
                      (dr/gaussian 0.85 0.07))
        :scale (dr/gaussian 1.0 0.2)
        :wiggle (if (dr/chance 0.2) (dr/gaussian 1.0 0.2) 0.0)
        :decay (dr/random 0.05 0.25)}))))

(defn generate-particles [boundary n color]
  (let [line0 (gl/line2 (cq/rel-vec -0.1 -0.15) (cq/rel-vec -0.1 1.15))
        line1 (gl/line2 (cq/rel-vec 1.1 -0.15) (cq/rel-vec 1.1 1.15))]
    (concat (repeatedly n (gen-particle line0 boundary color))
            (repeatedly n (gen-particle line1 boundary color)))))

(defn update-particles [particles dt t]
  (keep (move dt t 0.5 0.0003 0.99999) particles))

(defn inverted [x]
  (- 1.0 x))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [x ((dr/rand-nth [inverted identity])
           (dr/weighted {(/ 2 7) 1
                         (/ 2 5) 1
                         (/ 1 4) 1
                         (/ 1 3) 1}))
        line (gl/line2 (cq/rel-vec x -1.0) (cq/rel-vec x 2.0))
        angle (dr/gaussian 0.0 0.1)
        boundary (geometry/rotate-around-centroid line angle)
        ;; other factors requiring scaling, control for spin?
        scale (/ (q/height) 600)
        color (dr/weighted {[0.0 0.0 0.0] 3
                            [0.65 0.66 0.45] 1
                            [0.18 0.75 0.65] 1
                            [0.33 0.66 0.45] 1
                            [0.0 0.5 0.45] 1})]
    {:boundary boundary
     :particles (generate-particles boundary (int (* 100 scale)) color)
     :t 0.0}))

(defn update-state [{:keys [t] :as state}]
  (let [dt 0.1]
    (-> state
        (update :t + dt)
        (update :particles update-particles dt t))))

(defn draw-particle [{:keys [pos angle scale]} _t]
  (qdg/draw (triangle/inscribed-equilateral {:p pos :r (* scale 10)} angle)))

(defn draw [{:keys [particles t]}]
  (if (seq particles)
    (doseq [{:keys [color] :as particle} particles]
      (q/stroke 0.0 (tm/clamp (dr/gaussian 0.05 0.005) 0.025 0.1))
      (apply q/fill (conj color 0.01))
      (draw-particle particle t))
    (q/no-loop)))

(sketch/defquil liminal-tension
  :created-at "2023-03-21"
  :tags #{}
  :size [900 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
