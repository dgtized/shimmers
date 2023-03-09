(ns shimmers.sketches.superposition-mirrored
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.control :as control]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

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
                     :lr-in-triangles 1
                     :lr-out-triangles 1
                     :ud-in-triangles 1
                     :ud-out-triangles 1})
       0.2))
  ([kind scale]
   (case kind
     :center-circle
     [(gc/circle (cq/rel-vec 0.5 0.5) (cq/rel-h scale))]
     :ud-in-triangles
     [(triangle/inscribed-equilateral {:p (cq/rel-vec 0.5 0.2) :r (cq/rel-h scale)} (* eq/TAU 0.25))
      (triangle/inscribed-equilateral {:p (cq/rel-vec 0.5 0.8) :r (cq/rel-h scale)} (* eq/TAU 0.75))]
     :ud-out-triangles
     [(triangle/inscribed-equilateral {:p (cq/rel-vec 0.5 0.2) :r (cq/rel-h scale)} (* eq/TAU 0.75))
      (triangle/inscribed-equilateral {:p (cq/rel-vec 0.5 0.8) :r (cq/rel-h scale)} (* eq/TAU 0.25))]
     :lr-in-triangles
     [(triangle/inscribed-equilateral {:p (cq/rel-vec 0.15 0.5) :r (cq/rel-h scale)} 0)
      (triangle/inscribed-equilateral {:p (cq/rel-vec 0.85 0.5) :r (cq/rel-h scale)} Math/PI)]
     :lr-out-triangles
     [(triangle/inscribed-equilateral {:p (cq/rel-vec 0.15 0.5) :r (cq/rel-h scale)} Math/PI)
      (triangle/inscribed-equilateral {:p (cq/rel-vec 0.85 0.5) :r (cq/rel-h scale)} 0)])))

(defn move [dt]
  (fn [{:keys [pos angle vel angle-vel dest] :as particle}]
    (let [force (control/force-accel pos dest (/ 0.5 dt) vel)
          angle-target (g/heading (tm/- dest pos))
          angle-acc (control/angular-acceleration angle angle-target (/ 0.5 dt) angle-vel)]
      (-> particle
          (assoc
           :pos (tm/+ pos (tm/* vel dt))
           :angle (+ angle (* angle-vel dt))
           :vel (tm/+ vel (tm/* force dt))
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
     :particles (make-particles shapes 32)
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
    (cq/draw-polygon (triangle/inscribed-equilateral {:p pos :r (cq/rel-h 0.03)} angle))
    (q/line pos dest)))

(defn page []
  [:div
   (sketch/component
    :size [900 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:div.contained
    [:p.explanation.readable-width
     "Variation on superposition with more intentional shape placement."]]])

(sketch/definition superposition-mirrored
  {:created-at "2023-03-08"
   :tags #{}}
  (ctrl/mount page "sketch-host"))
