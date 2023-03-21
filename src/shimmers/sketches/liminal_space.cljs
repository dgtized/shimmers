(ns shimmers.sketches.liminal-space
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

(defrecord Particle [pos angle vel angle-vel dest decay])

(defn move [dt pos-c angle-c drag]
  (fn [{:keys [pos angle vel angle-vel dest decay] :as particle}]
    (let [force (control/force-accel pos dest pos-c vel)
          angle-target (g/heading (tm/- dest pos))
          angle-acc (if (< angle-c 90)
                      (control/angular-acceleration angle angle-target angle-c angle-vel)
                      (* 2 (- angle-c 90)))
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

(defn gen-particle [line boundary]
  (fn []
    (let [pos (g/point-at line (dr/random))]
      (->Particle pos
                  (dr/random-tau)
                  (dr/randvec2 10)
                  (dr/gaussian 0.0 1.0)
                  (tm/mix pos (g/closest-point boundary pos) (dr/random 0.8 0.95))
                  (dr/random 0.05 0.4)))))

(defn generate-particles [boundary n]
  (let [line0 (gl/line2 (cq/rel-vec -0.2 -0.1) (cq/rel-vec -0.2 1.1))
        line1 (gl/line2 (cq/rel-vec 1.2 -0.1) (cq/rel-vec 1.2 1.1))]
    (concat (repeatedly n (gen-particle line0 boundary))
            (repeatedly n (gen-particle line1 boundary)))))

(defn update-particles [particles dt]
  (keep (move dt 0.5 0.01 0.99999) particles))

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
        boundary (geometry/rotate-around-centroid line angle)]
    {:boundary boundary
     :particles (generate-particles boundary 100)
     :t 0.0}))

(defn update-state [state]
  (let [dt 0.1]
    (-> state
        (update :t + dt)
        (update :particles update-particles dt))))

(defn draw-particle [{:keys [pos angle]} _t]
  (qdg/draw (triangle/inscribed-equilateral {:p pos :r 8} angle)))

(defn draw [{:keys [particles t]}]
  (q/stroke 0.0 0.1)
  (q/fill 0.0 0.02)
  (doseq [particle particles]
    (draw-particle particle t)))

(sketch/defquil liminal-space
  :created-at "2023-03-21"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
