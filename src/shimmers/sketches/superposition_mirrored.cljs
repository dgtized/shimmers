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
  (let [random-point (dr/weighted {g/random-point 1
                                   g/random-point-inside 8})]
    (->> (fn []
           (random-point (dr/rand-nth shapes)))
         (repeatedly n))))

(defn make-particles [shapes n]
  (let [points (distribute-particles shapes n)
        dests (distribute-particles shapes n)]
    (map (fn [p d]
           (->Particle p (dr/random-tau) (gv/vec2) 0 d))
         points dests)))

;; IDEA: option to rotate shapes near a target around a common centroid?
(defn generate-shapes
  ([scale] (generate-shapes
            (dr/weighted {:bounds 0.5
                          :center-circle 1
                          :center-square 1
                          :center-hexagon 1
                          :quad-square 1
                          :lr-in-triangles 1
                          :lr-out-triangles 1
                          :ud-in-triangles 1
                          :ud-out-triangles 1})
            scale))
  ([kind scale]
   (let [r (cq/rel-h scale)]
     (case kind
       :bounds
       [(cq/screen-rect 0.95)]
       :center-circle
       [(gc/circle (cq/rel-vec 0.5 0.5) r)]
       :center-square
       [(g/center (rect/rect (* 2 r)) (cq/rel-vec 0.5 0.5))]
       :center-hexagon
       [(g/as-polygon (gc/circle (cq/rel-vec 0.5 0.5) r) 6)]
       :quad-square
       (let [rect (rect/rect (* 1.5 r))]
         [(g/center rect (cq/rel-vec 0.25 0.25))
          (g/center rect (cq/rel-vec 0.75 0.25))
          (g/center rect (cq/rel-vec 0.75 0.75))
          (g/center rect (cq/rel-vec 0.25 0.75))])
       :ud-in-triangles
       [(triangle/inscribed-equilateral {:p (cq/rel-vec 0.5 (* 1.1 scale)) :r r} (* eq/TAU 0.25))
        (triangle/inscribed-equilateral {:p (cq/rel-vec 0.5 (- 1.0 (* 1.1 scale))) :r r} (* eq/TAU 0.75))]
       :ud-out-triangles
       [(triangle/inscribed-equilateral {:p (cq/rel-vec 0.5 (* 1.1 scale)) :r r} (* eq/TAU 0.75))
        (triangle/inscribed-equilateral {:p (cq/rel-vec 0.5 (- 1.0 (* 1.1 scale))) :r r} (* eq/TAU 0.25))]
       :lr-in-triangles
       [(triangle/inscribed-equilateral {:p (cq/rel-vec scale 0.5) :r r} 0)
        (triangle/inscribed-equilateral {:p (cq/rel-vec (- 1.0 scale) 0.5) :r r} Math/PI)]
       :lr-out-triangles
       [(triangle/inscribed-equilateral {:p (cq/rel-vec scale 0.5) :r r} Math/PI)
        (triangle/inscribed-equilateral {:p (cq/rel-vec (- 1.0 scale) 0.5) :r r} 0)]))))

(defn move [dt pos-c angle-c drag]
  (fn [{:keys [pos angle vel angle-vel dest] :as particle}]
    (let [force (control/force-accel pos dest pos-c vel)
          angle-target (g/heading (tm/- dest pos))
          angle-acc (if (< angle-c 90)
                      (control/angular-acceleration angle angle-target angle-c angle-vel)
                      (* 2 (- angle-c 90)))
          drag-c (- 1.0 (eq/sqr (* drag dt)))]
      (-> particle
          (assoc
           :pos (tm/+ pos (tm/* vel dt))
           :angle (+ angle (* angle-vel dt))
           :vel (tm/limit (tm/* (tm/+ vel (tm/* force dt)) drag-c)
                          (+ (* 3 pos-c) (/ 1.0 dt)))
           :angle-vel (* (+ angle-vel (* angle-acc dt)) drag-c))))))

(defn update-positions [particles t dt]
  (mapv (move dt
              (+ 5 (* 150.0 (q/noise t 10.0)))
              (+ 5 (* 150.0 (q/noise 10.0 t)))
              (+ 1.0 (* 50.0 (q/noise 20.0 (* t dt 0.008)))))
        particles))

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

(defn update-state [{:keys [particles t] :as state}]
  (let [dt (dr/random 0.001 0.01)]
    (if (< (affinity particles) (cq/rel-h 0.005))
      (let [targets (generate-shapes (dr/random 0.05 0.49))]
        (-> state
            (update :t + dt)
            (assoc :shapes targets)
            (update :particles update-destinations targets)))
      (-> state
          (update :t + dt)
          (update :particles update-positions t dt)))))

(defonce ui-state (ctrl/state {:debug false}))

(defn draw [{:keys [image shapes particles t]}]
  (let [diagonal (g/dist (gv/vec2 0 0) (cq/rel-vec 0.5 0.5))
        scale (+ 0.002 (* 0.25 (Math/pow (q/noise (* t 0.15) 100.0) 3)))
        color (< 0.2 (q/noise (* t 0.1) 1000.0) 0.8)]
    (q/with-graphics image
      (q/color-mode :hsl 1.0)
      (q/fill 1.0 0.1)
      (q/stroke 0.0 0.1)
      (doseq [{:keys [pos angle]} particles]
        (let [r (* 2 (Math/pow (/ (g/dist pos (cq/rel-vec 0.5 0.5)) diagonal) tm/PHI))]
          (when color
            (q/fill (mod (* 3 (apply q/noise (tm/* (gv/vec2 t r) 0.01))) 1.0)
                    (+ 0.4 (* 0.6 (apply q/noise (tm/+ (tm/* (gv/vec2 (+ t r) r) 0.05) (gv/vec2 50.0 100.0)))))
                    (+ 0.45 (* 0.55 (apply q/noise (tm/+ (tm/* (gv/vec2 t r) 0.02) (gv/vec2 100.0 50.0)))))
                    (+ 0.001 (* 0.04 (apply q/noise (tm/+ (tm/* (gv/vec2 t r) 0.006) (gv/vec2 200.0 200.0))))))
            (q/stroke (mod (* 2 (apply q/noise (tm/* (gv/vec2 r (+ t r)) 0.0035))) 1.0)
                      (+ 0.001 (* 0.1 (apply q/noise (tm/+ (tm/* (gv/vec2 (+ angle t) (+ angle r)) 0.005) (gv/vec2 300.0 300.0)))))))
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
     "Variation on superposition with more intentional shape placement leveraging symmetries."]
    [:div.ui-controls
     (ctrl/checkbox ui-state "Debug" [:debug])]]])

(sketch/definition superposition-mirrored
  {:created-at "2023-03-08"
   :tags #{}}
  (ctrl/mount page "sketch-host"))
