(ns shimmers.sketches.superposition-mirrored
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.linear-assignment :as linear]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.screen :as screen]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.control :as control]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.arc :as arc]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def defo (debug/state))

(defn clock []
  (/ (q/millis) 3333.0))

(defn center-filter
  "Map a noise value as distance from center point into [0.0,1.0].

  If distance is less than `percent-cutoff` output is 0.0"
  [percent-cutoff n]
  (tm/map-interval-clamped
   (abs (- n 0.5))
   (* percent-cutoff 0.5) 0.5
   0.0 1.0))

(defrecord Particle [pos angle vel angle-vel dest])

(defn select-random-point [bias]
  (dr/weighted {:outside (- 1 bias)
                :inside bias}))

(def random-point-fn
  {:outside rp/sample-point-bounds
   :inside rp/sample-point-inside})

(defn init-linear-matrix [particles {:keys [random-point shapes]}]
  (let [rp (random-point-fn random-point)
        distribute-randomly (fn [n] (repeatedly n #(rp (dr/rand-nth shapes))))
        positions
        (if (dr/chance 0.6)
          (distribute-randomly (count particles))
          ;; distribute particles per shape evenly
          (let [per-shape (int (/ (count particles) (count shapes)))]
            (concat (mapcat (fn [shape] (repeatedly per-shape #(rp shape))) shapes)
                    (distribute-randomly (mod (count particles) per-shape)))))]
    (assoc (linear/online-match-matrix < (mapv :dest particles) (into [] positions))
           :shapes shapes)))

(defn distribute-particles [{:keys [random-point shapes]} particles]
  (let [rp (random-point-fn random-point)
        max-dist (g/dist (cq/rel-vec 0 0) (cq/rel-vec 1 1))]
    (map (fn [{:keys [pos]}]
           (rp (dr/weighted-by (fn [s] (- max-dist (g/dist pos (g/centroid s))))
                               shapes)))
         particles)))

(defn make-particles [point-gen n]
  (let [init (repeatedly n (fn [] {:pos (gv/vec2)}))
        points (distribute-particles point-gen init)
        dests (distribute-particles point-gen init)]
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

(defn point-gen
  ([shapes] (point-gen (select-random-point (/ 8 9)) shapes))
  ([random-point shapes]
   {:random-point random-point
    :shapes shapes}))

(defn triangles-on-vertices [n r face]
  (let [hex-points (-> (cq/rel-vec 0.5 0.5)
                       (gc/circle (* 0.9 r))
                       (g/as-polygon n)
                       g/vertices)]
    (for [point hex-points]
      (triangle/inscribed-equilateral
       {:p point :r (* 0.3 r)}
       (g/heading (if (= face :inside)
                    (tm/- point (cq/rel-vec 0.5 0.5))
                    (tm/- (cq/rel-vec 0.5 0.5) point)))))))

;; IDEA: option to rotate shapes near a target around a common centroid?
(defn generate-shapes
  ([] (generate-shapes
       (dr/weighted {:bounds 0.4
                     :center-circle 1
                     :arc 1
                     :center-square 1
                     :center-hexagon 1
                     :quad-square 1
                     :quad-triangle 1
                     :lr-in-triangles 1
                     :lr-out-triangles 1
                     :ud-in-triangles 1
                     :ud-out-triangles 1
                     :vertical-bars 1
                     :horizontal-bars 1
                     :hex-triangles 0.5
                     :oct-triangles 0.5})
       (dr/random 0.1 0.49)))
  ([kind scale]
   (let [r (cq/rel-h scale)]
     (case kind
       :bounds
       (point-gen [(cq/screen-rect 0.95)])
       :center-circle
       (point-gen [(gc/circle (cq/rel-vec 0.5 0.5) r)])
       :arc
       (let [base (dr/weighted {(* 0.75 eq/TAU) 1
                                (* 0.0 eq/TAU) 1})
             center (cq/rel-vec 0.5 0.5)]
         (point-gen
          (select-random-point (/ 1 5))
          [(arc/arc center r (+ base (* (/ 1 8) eq/TAU)) (+ base (* (/ 3 8) eq/TAU)))
           (arc/arc center r (+ base (* (/ 5 8) eq/TAU)) (+ base (* (/ 7 8) eq/TAU)))]))
       :center-square
       (point-gen [(g/center (rect/rect (* 2 r)) (cq/rel-vec 0.5 0.5))])
       :center-hexagon
       (point-gen
        (select-random-point (/ 1 3))
        [(g/as-polygon (gc/circle (cq/rel-vec 0.5 0.5) r) 6)])
       :quad-square
       (let [rect (rect/rect (* 1.5 r))]
         (point-gen
          [(g/center rect (cq/rel-vec 0.25 0.25))
           (g/center rect (cq/rel-vec 0.75 0.25))
           (g/center rect (cq/rel-vec 0.75 0.75))
           (g/center rect (cq/rel-vec 0.25 0.75))]))
       :quad-triangle
       (point-gen
        (quad-shape 0.0 (g/center (triangle/inscribed-equilateral {:p (gv/vec2) :r r} 0))))
       :horizontal-bars
       (point-gen [(rect/rect (cq/rel-vec 0.05 0.05) (cq/rel-vec 0.95 0.2))
                   (rect/rect (cq/rel-vec 0.05 0.8) (cq/rel-vec 0.95 0.95))])
       :vertical-bars
       (point-gen [(rect/rect (cq/rel-vec 0.05 0.05) (cq/rel-vec 0.2 0.95))
                   (rect/rect (cq/rel-vec 0.8 0.05) (cq/rel-vec 0.95 0.95))])
       :ud-in-triangles
       (point-gen
        [(triangle/inscribed-equilateral {:p (cq/rel-vec 0.5 (* 1.1 scale)) :r r} (* eq/TAU 0.25))
         (triangle/inscribed-equilateral {:p (cq/rel-vec 0.5 (- 1.0 (* 1.1 scale))) :r r} (* eq/TAU 0.75))])
       :ud-out-triangles
       (point-gen
        [(triangle/inscribed-equilateral {:p (cq/rel-vec 0.5 (* 1.1 scale)) :r r} (* eq/TAU 0.75))
         (triangle/inscribed-equilateral {:p (cq/rel-vec 0.5 (- 1.0 (* 1.1 scale))) :r r} (* eq/TAU 0.25))])
       :lr-in-triangles
       (point-gen
        [(triangle/inscribed-equilateral {:p (cq/rel-vec scale 0.5) :r r} 0)
         (triangle/inscribed-equilateral {:p (cq/rel-vec (- 1.0 scale) 0.5) :r r} math/PI)])
       :lr-out-triangles
       (point-gen
        [(triangle/inscribed-equilateral {:p (cq/rel-vec scale 0.5) :r r} math/PI)
         (triangle/inscribed-equilateral {:p (cq/rel-vec (- 1.0 scale) 0.5) :r r} 0)])
       :hex-triangles
       (point-gen (select-random-point (/ 1 11))
                  (triangles-on-vertices 6 r (dr/rand-nth [:inside :outside])))
       :oct-triangles
       (point-gen (select-random-point (/ 1 11))
                  (triangles-on-vertices 8 r (dr/rand-nth [:inside :outside])))))))

(defn perp-motion [pos dest wobble]
  (let [v (tm/- pos dest)]
    (g/rotate (tm/normalize v wobble) (* 0.25 eq/TAU))))

;; TODO: add repulsion zones, though would need to ensure destinations were outside?
(defn move [dt {:keys [wobble pos-c steering angle-c target-vel drag]}]
  (fn [{:keys [pos angle vel angle-vel dest] :as particle}]
    (let [force (tm/+ (control/force-accel pos dest pos-c vel)
                      (tm/* (perp-motion pos dest wobble)
                            (tm/mag vel)))
          angle-acc (case steering
                      :active (control/angular-acceleration angle (g/heading (tm/- dest pos)) angle-c angle-vel)
                      :spin (control/spin-acceleration angle-vel target-vel angle-c))]
      (-> particle
          (assoc
           :pos (tm/+ pos (tm/* vel dt))
           :angle (+ angle (* angle-vel dt))
           :vel (tm/limit (tm/* (tm/+ vel (tm/* force dt)) drag)
                          (+ (* 10 pos-c) (/ 1.0 dt)))
           :angle-vel (* (+ angle-vel (* angle-acc dt)) drag))))))

(defn update-positions [particles t dt]
  (let [controls
        {:wobble (let [amp-n (center-filter 0.3 (q/noise 120.0 30.0 (* 0.25 t)))
                       amplitude (* (cq/rel-h 0.06) (dec (math/pow 2.0 amp-n)))
                       rate-n (+ 0.01 (center-filter 0.0 (q/noise 60.0 (* 0.1 t) 20.0)))
                       rate (* 8 (math/pow rate-n 2))]
                   (* amplitude (math/sin (* rate t))))
         :pos-c (+ 4 (* 150.0 (math/pow (center-filter 0.0 (q/noise (* t 0.2) 10.0)) 2)))
         :steering (let [steer-noise (center-filter 0.0 (q/noise (* (/ 3 7) t) 160))]
                     (if (< steer-noise 0.15)
                       :active
                       :spin))
         :angle-c (+ 4 (* 150.0 (math/pow (center-filter 0.0 (q/noise 10.0 (* t 0.2))) 2)))
         :target-vel (let [vel (* 2 (- (q/noise 250 (* 0.05 t)) 0.5))]
                       (+ (* (tm/sign vel) 2) (* 512 (math/pow vel 3))))
         :drag (- 1.0 (eq/sqr (* dt (+ 0.1 (* 100.0 (center-filter 0.1 (q/noise 20.0 (* 0.3 t))))))))}]
    (swap! defo assoc :controls controls)
    (mapv (move dt controls) particles)))

(defn update-destinations [particles matrix]
  (map (fn [particle [_ dest']]
         (assoc particle :dest dest'))
       particles
       (linear/online-match-solution matrix)))

(defn affinity [particles]
  (/ (reduce (fn [acc {:keys [pos dest]}]
               (+ acc (g/dist-squared pos dest)))
             0.0
             particles)
     (eq/sqr (count particles))))

(defn setup []
  ;; Performance, removes calls to addType & friends
  ;; now dominated by MinorGC and cost of sort?
  (set! (.-disableFriendlyErrors js/p5) true)

  (q/noise-seed (dr/seed))
  (q/color-mode :hsl 1.0)
  (let [{:keys [shapes] :as point-gen} (generate-shapes)
        particles (make-particles point-gen 128)
        image (q/create-graphics (q/width) (q/height))]
    (q/with-graphics image (q/color-mode :hsl 1.0))
    {:image image
     :shapes shapes
     :particles particles
     :linear-matrix (init-linear-matrix particles (generate-shapes))
     :cycle 0
     :t (clock)}))

(defonce ui-state (ctrl/state {:debug false
                               :mode :infinite
                               :paused false
                               :limit 20
                               :screen-size "900x600"}))

(defn running? [cycle]
  (let [{:keys [limit mode paused]} @ui-state]
    (and (not paused)
         (or (= mode :infinite)
             (< cycle limit)))))

(defn update-state [{:keys [particles t cycle linear-matrix] :as state}]
  (if (running? cycle)
    (let [t' (clock)
          ;; remove discontinuity in force from excessive dt
          ;; without this, suspending the tab would apply full
          ;; However, it did give an interesting side-effect of kicking all the particles out of view.
          dt (min (- t' t) 0.1)]
      (if (< (affinity particles) (cq/rel-h 0.05))
        (let [point-gen (generate-shapes)]
          (-> state
              (update :t + dt)
              (assoc :shapes (:shapes linear-matrix))
              (update :cycle inc)
              (assoc :linear-matrix (init-linear-matrix particles point-gen))
              (update :particles update-destinations linear-matrix)))
        (-> state
            (assoc :t t')
            (update :linear-matrix linear/online-match-update (* 2 (count particles)))
            (update :particles update-positions t dt))))
    state))

(defn noise-at [pos rate base]
  (let [[x y] (tm/+ (tm/* (gv/vec2 pos) rate) (gv/vec2 base))]
    (q/noise x y)))

(defn draw-image [{:keys [cycle particles t]}]
  (let [diagonal (g/dist (gv/vec2 0 0) (cq/rel-vec 0.5 0.5))
        scale-noise (center-filter 0.0 (q/noise (* t 0.3) 100.0))
        scale (cq/rel-h (+ 0.005 (* 0.10 (math/pow scale-noise 2))))
        color (< 0.2 (q/noise (* t 0.25) 1000.0) 0.8)
        individuality (center-filter 0.4 (q/noise 12000.0 (* t 0.05)))]
    (swap! defo assoc
           :cycle cycle
           :t t
           :draw {:scale-noise scale-noise
                  :scale scale
                  :color color
                  :individuality individuality})
    (doseq [{:keys [pos angle]} particles]
      (let [r (* 2 (math/pow (/ (g/dist pos (cq/rel-vec 0.5 0.5)) diagonal) tm/PHI))
            t (+ t
                 (if (> individuality 0)
                   (let [local-noise (noise-at (tm/+ (gv/vec2 t t) pos) 0.002 [800 900])]
                     (* (math/pow 4 individuality)
                        (math/pow (+ 1 (center-filter 0.25 local-noise)) 4)))
                   0))
            fill-opacity (- 1.0 (center-filter 0.0 (noise-at [t r] 0.006 [200.0 200.0])))
            stroke-opacity (center-filter 0.01 (noise-at [(+ r t) (+ r t)] 0.03 [300.0 300.0]))]
        (q/stroke (tm/smoothstep* 0.45 0.55 (noise-at [r (+ t r)] 0.15 [500.0 500.0]))
                  (+ 0.001 (* 0.25 (tm/smoothstep* 0.1 0.9 stroke-opacity))))
        (if color
          (q/fill (mod (* 3 (noise-at [(* 0.75 t) (eq/sqr r)] 0.01 [0 0])) 1.0)
                  (+ 0.45 (* 0.55 (noise-at [(+ t r) r] 0.15 [50.0 100.0])))
                  (+ 0.4 (* 0.5 (noise-at [r t] 0.1 [100.0 50.0])))
                  (+ 0.001 (* 0.05 fill-opacity)))
          (q/fill 1.0 (+ 0.01 (* 0.2 fill-opacity))))
        (cq/draw-polygon (triangle/inscribed-equilateral {:p pos :r scale} angle))))))

(defn debug-view [{:keys [shapes particles]}]
  (q/no-fill)
  (q/stroke 0.1 0.5)
  (doseq [poly shapes]
    (cq/draw-polygon poly))

  (doseq [{:keys [pos angle dest]} particles]
    (cq/draw-polygon (triangle/inscribed-equilateral {:p pos :r (cq/rel-h 0.03)} angle))
    (q/line pos dest)))

(defn draw [{:keys [image cycle] :as state}]
  (when (running? cycle)
    (q/with-graphics image
      (draw-image state)))

  (q/color-mode :hsl 1.0)
  (q/background 1.0)
  (q/image image 0 0)
  (when (:debug @ui-state)
    (debug-view state)))

(defn page []
  [sketch/with-explanation
   (sketch/component
     :size (screen/parse-size (:screen-size @ui-state))
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   [:div.flexcols
    [:div
     [:p.readable-width
      "Variation on superposition with more intentional shape placement
      leveraging symmetries. The motion for each triangle uses a control system
      to change velocity, orientation, and acceleration. This allows for more
      varied motion planning that can still adjust to temporary stimuli
      changes."]
     (let [mode (:mode @ui-state)]
       [ctrl/container
        [ctrl/dropdown ui-state "Screen Size" [:screen-size]
         (screen/sizes)
         {:on-change #(view-sketch/restart-sketch :superposition-mirrored)}]
        [ctrl/checkbox ui-state "Debug" [:debug]]
        [ctrl/checkbox ui-state "Paused" [:paused]]
        [ctrl/change-mode ui-state [:infinite :limit]]
        (when (= mode :limit)
          [ctrl/numeric ui-state "Limit Cycles" [:limit] [0 10000 1]])])]
    [:div {:style {:width "12em"}}
     [:p]
     (debug/display defo)]]])

(sketch/definition superposition-mirrored
  {:created-at "2023-03-08"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
