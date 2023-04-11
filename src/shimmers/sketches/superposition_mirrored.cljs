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
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn center-filter
  "Map a noise value as distance from center point into [0.0,1.0].

  If distance is less than `percent-cutoff` output is 0.0"
  [percent-cutoff n]
  (tm/map-interval-clamped (Math/abs (- n 0.5))
                           [(* percent-cutoff 0.5) 0.5]
                           [0.0 1.0]))

(defrecord Particle [pos angle vel angle-vel dest])

(defn distribute-particles [shapes points]
  (let [random-point (dr/weighted {g/random-point 1
                                   g/random-point-inside 8})]
    (map (fn [{:keys [pos]}]
           (random-point (dr/weighted-by (fn [s] (g/dist pos (g/centroid s)))
                                         shapes)))
         points)))

(defn make-particles [shapes n]
  (let [init (repeatedly n (fn [] {:pos (gv/vec2)}))
        points (distribute-particles shapes init)
        dests (distribute-particles shapes init)]
    (map (fn [p d]
           (->Particle p (dr/random-tau) (gv/vec2) 0 d))
         points dests)))

(defn circumcircle [{:keys [points]}]
  (apply gt/circumcircle points))

(defn quad-shape [d shape]
  (let [bounds (cq/screen-rect 0.98)
        center (g/centroid bounds)
        r (:r (circumcircle shape))
        corner-dist (g/dist (gv/vec2 0 0) center)
        facing (dr/rand-nth [(fn [a b] (tm/- a b))
                             (fn [a b] (tm/- b a))])]
    (for [corner (g/vertices bounds)]
      (-> shape
          (g/rotate (g/heading (facing corner center)))
          (g/center (tm/mix (tm/mix center corner (/ (- corner-dist (* tm/SQRT3 r)) corner-dist))
                            center d))))))

;; IDEA: option to rotate shapes near a target around a common centroid?
(defn generate-shapes
  ([scale] (generate-shapes
            (dr/weighted {:bounds 0.5
                          :center-circle 1
                          :center-square 1
                          :center-hexagon 1
                          :quad-square 1
                          :quad-triangle 1
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
       :quad-triangle
       (quad-shape 0.0 (g/center (triangle/inscribed-equilateral {:p (gv/vec2) :r r} 0)))
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

(defn perp-motion [pos dest wobble]
  (let [v (tm/- pos dest)]
    (g/rotate (tm/normalize v wobble) (* 0.25 eq/TAU))))

(defn move [dt wobble pos-c angle-c drag]
  (fn [{:keys [pos angle vel angle-vel dest] :as particle}]
    (let [force (tm/+ (control/force-accel pos dest pos-c vel)
                      (tm/* (perp-motion pos dest wobble)
                            (tm/mag vel)))
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
  (let [wobble
        (let [amp-n (center-filter 0.1 (q/noise 30.0 30.0 (* 0.66 t)))
              amplitude (* (cq/rel-h 0.1) (dec (Math/pow 2.0 amp-n)))
              rate-n (center-filter 0.1 (q/noise 60.0 (* 0.1 t) 20.0))
              rate (tm/mix* 8 32 rate-n)]
          (* amplitude (Math/sin (* rate t))))]
    (mapv (move dt wobble
                (+ 5 (* 150.0 (q/noise t 10.0)))
                (+ 5 (* 150.0 (q/noise 10.0 t)))
                ;; FIXME: does this drag make any sense?
                (+ 1.0 (* 50.0 (q/noise 20.0 (* t dt 0.008)))))
          particles)))

(defn update-destinations [particles targets]
  (map (fn [particle dest]
         (assoc particle :dest dest))
       particles
       (distribute-particles targets particles)))

(defn affinity [particles]
  (/ (reduce (fn [acc {:keys [pos dest]}]
               (+ acc (g/dist pos dest)))
             0.0
             particles)
     (count particles)))

(defn setup []
  (q/noise-seed (dr/random-int 100000))
  (q/color-mode :hsl 1.0)
  (let [shapes (generate-shapes (dr/random 0.1 0.49))]
    {:image (q/create-graphics (q/width) (q/height))
     :shapes shapes
     :particles (make-particles shapes 128)
     :cycle 0
     :t 0.0}))

(defonce ui-state (ctrl/state {:debug false
                               :mode :infinite
                               :paused false
                               :limit 20}))

(defn running? [cycle]
  (let [{:keys [limit mode paused]} @ui-state]
    (and (not paused)
         (or (= mode :infinite)
             (< cycle limit)))))

(defn update-state [{:keys [particles t cycle] :as state}]
  (if (running? cycle)
    (let [dt (dr/random 0.001 0.01)]
      (if (< (affinity particles) (cq/rel-h 0.005))
        (let [targets (generate-shapes (dr/random 0.05 0.49))]
          (-> state
              (update :t + dt)
              (assoc :shapes targets)
              (update :cycle inc)
              (update :particles update-destinations targets)))
        (-> state
            (update :t + dt)
            (update :particles update-positions t dt))))
    state))

(defn noise-at [pos rate base]
  (let [[x y] (tm/+ (tm/* (gv/vec2 pos) rate) (gv/vec2 base))]
    (q/noise x y)))

(defn draw [{:keys [image cycle shapes particles t]}]
  (when (running? cycle)
    (let [diagonal (g/dist (gv/vec2 0 0) (cq/rel-vec 0.5 0.5))
          scale-noise (center-filter 0.0 (q/noise (* t 0.2) 100.0))
          scale (cq/rel-h (+ 0.005 (* 0.09 (tm/smoothstep* -0.1 1.33 scale-noise))))
          color (< 0.2 (q/noise (* t 0.1) 1000.0) 0.8)]
      (q/with-graphics image
        (q/color-mode :hsl 1.0)
        (q/fill 1.0 0.1)
        (q/stroke 0.0 0.1)
        (doseq [{:keys [pos angle]} particles]
          (let [r (* 2 (Math/pow (/ (g/dist pos (cq/rel-vec 0.5 0.5)) diagonal) tm/PHI))
                fill-opacity (- 1.0 (center-filter 0.0 (noise-at [t r] 0.006 [200.0 200.0])))
                stroke-opacity (- 1.0 (center-filter 0.0 (noise-at [(+ r t) (+ r t)] 0.005 [300.0 300.0])))]
            (when color
              (q/fill (mod (* 3 (noise-at [(* 0.75 t) (eq/sqr r)] 0.01 [0 0])) 1.0)
                      (+ 0.4 (* 0.6 (noise-at [(+ t r) r] 0.05 [50.0 100.0])))
                      (+ 0.45 (* 0.55 (noise-at [r t] 0.02 [100.0 50.0])))
                      (+ 0.001 (* 0.04 fill-opacity)))
              (q/stroke (tm/smoothstep* 0.4 0.6 (noise-at [r (+ t r)] 0.1 [500.0 500.0]))
                        (+ 0.001 (* 0.1 stroke-opacity))))
            (cq/draw-polygon (triangle/inscribed-equilateral {:p pos :r scale} angle)))))))

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
    (let [mode (:mode @ui-state)]
      [:div.ui-controls
       (ctrl/checkbox ui-state "Debug" [:debug])
       (ctrl/checkbox ui-state "Paused" [:paused])
       (ctrl/change-mode ui-state [:infinite :limit])
       (when (= mode :limit)
         (ctrl/numeric ui-state "Limit Cycles" [:limit] [0 10000 1]))])]])

(sketch/definition superposition-mirrored
  {:created-at "2023-03-08"
   :tags #{}}
  (ctrl/mount page "sketch-host"))
