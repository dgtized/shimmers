(ns shimmers.sketches.superposition-mirrored
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [shimmers.math.control :as control]))

(defrecord Particle [pos angle vel angle-vel dest])

(defn distribute-particles [shapes n]
  (->> (fn []
         (g/random-point-inside (dr/rand-nth shapes)))
       (repeatedly n)))

(defn make-particles [shapes n]
  (let [points (distribute-particles shapes n)
        dests (distribute-particles shapes n)]
    (map (fn [p d]
           (->Particle p (dr/random-tau) (gv/vec2) 0 d))
         points dests)))

(defn generate-shapes
  ([] (generate-shapes
       (dr/weighted {:center-circle 1
                     :lr-out-triangles 1})))
  ([kind]
   (case kind
     :center-circle
     [(gc/circle (cq/rel-vec 0.5 0.5) (cq/rel-h 0.2))]
     :lr-out-triangles
     [(triangle/inscribed-equilateral {:p (cq/rel-vec 0.15 0.5) :r (cq/rel-h 0.15)} Math/PI)
      (triangle/inscribed-equilateral {:p (cq/rel-vec 0.85 0.5) :r (cq/rel-h 0.15)} 0)])))

(defn move [dt]
  (fn [{:keys [pos angle vel angle-vel dest] :as particle}]
    (let [angle-target (g/heading (tm/- dest pos))
          angle-acc (control/angular-acceleration angle angle-target 0.01 angle-vel)]
      (-> particle
          (assoc
           :pos (tm/+ pos (tm/* vel dt))
           :angle (+ angle (* angle-vel dt))
           :vel (tm/+ vel (tm/* (control/force-accel pos dest 0.1 vel) dt))
           :angle-vel (+ angle-vel (* angle-acc dt)))))))

(defn update-positions [particles dt]
  (mapv (move dt) particles))

(defn update-destinations [particles targets]
  (map (fn [particle dest]
         (assoc particle :dest dest))
       particles
       (distribute-particles targets (count particles))))

(defn affinity [particles]
  (/ (reduce (fn [acc {:keys [pos dest]}]
               (+ acc (g/dist pos dest)))
             0.0
             particles)
     (count particles)))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [shapes (generate-shapes)]
    {:shapes shapes
     :particles (make-particles shapes 10)
     :t 0.0}))

(defn update-state [{:keys [particles] :as state}]
  (let [dt 0.01]
    (if (< (affinity particles) (cq/rel-h 0.01))
      (let [targets (generate-shapes)]
        (-> state
            (update :t + dt)
            (assoc :shapes targets)
            (update :particles update-destinations targets)))
      (-> state
          (update :t + dt)
          (update :particles update-positions dt)))))

(defn draw [{:keys [shapes particles]}]
  (q/background 1.0)
  (doseq [poly shapes]
    (cq/draw-polygon poly))

  (doseq [{:keys [pos angle dest]} particles]
    (cq/draw-polygon (triangle/inscribed-equilateral {:p pos :r (cq/rel-h 0.02)} angle))
    (q/line pos dest)))

(sketch/defquil superposition-mirrored
  :created-at "2023-03-08"
  :tags #{}
  :size [900 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
