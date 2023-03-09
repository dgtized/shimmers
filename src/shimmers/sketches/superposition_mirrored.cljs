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
   [thi.ng.geom.rect :as rect]
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
  ([scale] (generate-shapes
            (dr/weighted {:center-circle 1
                          :center-square 1
                          :quad-square 1
                          :lr-in-triangles 1
                          :lr-out-triangles 1
                          :ud-in-triangles 1
                          :ud-out-triangles 1})
            scale))
  ([kind scale]
   (let [r (cq/rel-h scale)]
     (case kind
       :center-circle
       [(gc/circle (cq/rel-vec 0.5 0.5) r)]
       :center-square
       [(g/center (rect/rect (* 2 r)) (cq/rel-vec 0.5 0.5))]
       :quad-square
       [(g/center (rect/rect r) (cq/rel-vec 0.25 0.25))
        (g/center (rect/rect r) (cq/rel-vec 0.75 0.25))
        (g/center (rect/rect r) (cq/rel-vec 0.75 0.75))
        (g/center (rect/rect r) (cq/rel-vec 0.25 0.75))]
       :ud-in-triangles
       [(triangle/inscribed-equilateral {:p (cq/rel-vec 0.5 (* 1.25 scale)) :r r} (* eq/TAU 0.25))
        (triangle/inscribed-equilateral {:p (cq/rel-vec 0.5 (- 1.0 (* 1.25 scale))) :r r} (* eq/TAU 0.75))]
       :ud-out-triangles
       [(triangle/inscribed-equilateral {:p (cq/rel-vec 0.5 (* 1.25 scale)) :r r} (* eq/TAU 0.75))
        (triangle/inscribed-equilateral {:p (cq/rel-vec 0.5 (- 1.0 (* 1.25 scale))) :r r} (* eq/TAU 0.25))]
       :lr-in-triangles
       [(triangle/inscribed-equilateral {:p (cq/rel-vec scale 0.5) :r r} 0)
        (triangle/inscribed-equilateral {:p (cq/rel-vec (- 1.0 scale) 0.5) :r r} Math/PI)]
       :lr-out-triangles
       [(triangle/inscribed-equilateral {:p (cq/rel-vec scale 0.5) :r r} Math/PI)
        (triangle/inscribed-equilateral {:p (cq/rel-vec (- 1.0 scale) 0.5) :r r} 0)]))))

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
  (q/noise-seed (dr/random-int 100000))
  (q/color-mode :hsl 1.0)
  (let [shapes (generate-shapes 0.2)]
    {:image (q/create-graphics (q/width) (q/height))
     :shapes shapes
     :particles (make-particles shapes 128)
     :t 0.0}))

(defn update-state [{:keys [particles] :as state}]
  (let [dt (dr/random 0.001 0.01)]
    (if (< (affinity particles) (cq/rel-h 0.01))
      (let [targets (generate-shapes (dr/random 0.05 0.45))]
        (-> state
            (update :t + dt)
            (assoc :shapes targets)
            (update :particles update-destinations targets)))
      (-> state
          (update :t + dt)
          (update :particles update-positions dt)))))

(defonce ui-state (ctrl/state {:debug false}))

(defn draw [{:keys [image shapes particles t]}]
  (let [diagonal (g/dist (gv/vec2 0 0) (cq/rel-vec 0.5 0.5))
        scale (+ 0.002 (* 0.25 (Math/pow (q/noise (* t 0.1) 100.0) 3.5)))]
    (q/with-graphics image
      (q/color-mode :hsl 1.0)
      (doseq [{:keys [pos angle]} particles]
        (let [r (/ (g/dist pos (cq/rel-vec 0.5 0.5)) diagonal)]
          (q/fill (mod (* tm/PHI (apply q/noise (tm/* (gv/vec2 t r) 0.02))) 1.0)
                  (+ 0.4 (* 0.6 (apply q/noise (tm/+ (tm/* (gv/vec2 (+ t r) r) 0.1) (gv/vec2 50.0 100.0)))))
                  (+ 0.45 (* 0.55 (apply q/noise (tm/+ (tm/* (gv/vec2 t r) 0.02) (gv/vec2 100.0 50.0)))))
                  (+ 0.001 (* 0.04 (apply q/noise (tm/+ (tm/* (gv/vec2 t r) 0.005) (gv/vec2 200.0 200.0))))))
          (q/stroke (mod (* 2 (apply q/noise (tm/* (gv/vec2 r (+ t r)) 0.5))) 1.0) 0.05)
          (cq/draw-polygon (triangle/inscribed-equilateral {:p pos :r (cq/rel-h scale)} angle))))))

  (q/color-mode :hsl 1.0)
  (q/background 1.0)
  (q/image image 0 0)

  (when (:debug @ui-state)
    (doseq [poly shapes]
      (cq/draw-polygon poly))

    (doseq [{:keys [pos angle dest]} particles]
      (cq/draw-polygon (triangle/inscribed-equilateral {:p pos :r (cq/rel-h 0.03)} angle))
      (q/line pos dest))))

(defn page []
  [:div
   (sketch/component
    :size [900 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:div.contained.explanation
    [:p.readable-width
     "Variation on superposition with more intentional shape placement."]
    [:div.ui-controls
     (ctrl/checkbox ui-state "Debug" [:debug])]]])

(sketch/definition superposition-mirrored
  {:created-at "2023-03-08"
   :tags #{}}
  (ctrl/mount page "sketch-host"))
