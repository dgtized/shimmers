(ns shimmers.sketches.superposition
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq :refer [rel-h rel-w]]
            [shimmers.common.transition-interval :as transition]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.common.ui.debug :as debug]
            [shimmers.math.deterministic-random :as dr]
            [shimmers.math.equations :as eq]
            [shimmers.math.geometry :as geometry]
            [shimmers.math.probability :as p]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.bezier :as bezier]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.utils :as gu]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defonce defo (debug/state))

;; Represent a brush stroke from location p to q
;; caches the resulting spline and index of positions
(defrecord Stroke [p q curve spline arc-length-idx]
  g/ISample
  (point-at
    [_ t] (gu/point-at t (get spline :points) arc-length-idx)))

(defn make-stroke
  ([p q] (make-stroke p q 0))
  ([p q d]
   (let [curve (geometry/confused-midpoint p q d)
         spline (bezier/auto-spline2 [p curve q])]
     (Stroke. p q curve spline
              (gu/arc-length-index (:points spline))))))

(defonce ui-state (ctrl/state {:debug false}))

(defn brush-at [stroke [radius freq] t]
  (tm/+ (gv/vec2)
        (gv/vec2 radius (* t freq)) ;; rotate around origin/path
        ;; point-at is expensive on splines, can this be precomputed?
        (g/point-at stroke t)))

(def triangle-instance
  (let [t (gt/triangle2 [0 0] [0 13] [17 0])]
    (assoc (g/center t)
           :centroid (gu/centroid (:points t)))))

;; optimized version with cached centroid, also trying to reduce consing?
(defn draw-brush-cohort [cohort scale orbit tween spin]
  (let [{:keys [points centroid]} triangle-instance]
    (q/begin-shape :triangles)
    (doseq [brush cohort
            :let [position (brush-at brush orbit tween)
                  theta (if spin
                          (* spin tween)
                          (* 2 Math/PI (rand)))]]
      (doseq [p points]
        (apply q/vertex
               (-> p
                   (tm/madd scale centroid)
                   (g/rotate theta)
                   (tm/+ position)))))
    (q/end-shape)))

(defn random-triangle []
  (let [s (dr/random 0.15 0.5)
        r [0.2 0.8]]
    (-> (gt/triangle2 [0 0] [0 (rel-h s)] [(rel-w s) 0])
        (geometry/rotate-around-centroid (dr/random eq/TAU))
        (g/translate (cq/rel-pos (apply dr/random r)
                                 (apply dr/random r))))))

(defn random-rect []
  (let [w (dr/random 0.15 0.5)
        h (dr/random 0.15 0.5)]
    (-> (rect/rect (rel-w (* (- 1 w) (dr/random-double)))
                   (rel-h (* (- 1 h) (dr/random-double)))
                   (rel-w w) (rel-h h))
        (geometry/rotate-around-centroid (dr/random eq/TAU)))))

(defn random-circle []
  (let [r (dr/random 0.05 0.35)]
    (gc/circle (cq/rel-pos (tm/clamp (dr/random-double) r (- 1 r))
                           (tm/clamp (dr/random-double) r (- 1 r)))
               (rel-h r))))

(defn random-target []
  ((dr/rand-nth [random-rect random-circle random-triangle])))

(defn var-rate [n]
  (Math/sin (* (/ Math/PI 2) n)))

(defn orbit-transition
  "Transition from old orbit to new in the first 20% of the motion from A to B.

  Reduces discontinuities when brush moves into a new orbit."
  [{:keys [orbit tween]}]
  (tm/mix (first orbit) (second orbit) (tm/smoothstep* 0 0.2 tween)))

(defn transition-to
  [{:keys [brushes cohorts orbit]
    previous :target
    :as state}
   fc target]
  (let [curve (* 0.8 (p/happensity 0.4))
        last-orbit (last orbit)
        random-point-from
        (dr/weighted {g/random-point-inside 8
                      g/random-point 1})
        brushes' (->> brushes
                      (map-indexed
                       (fn [idx brush]
                         (let [p (brush-at brush last-orbit 1.0)
                               q (random-point-from target)]
                           (assoc (make-stroke p q curve)
                                  :cohort (mod idx cohorts)))))
                      (sort-by :cohort))]
    (assoc state :current previous
           :target target
           :brushes brushes'
           :brush-cohorts (partition-by :cohort brushes')
           :variance [cohorts (* 20 (dr/gaussian 0 1))]
           :transition (transition/after fc (dr/random-int 120 600))
           :spin (when (dr/chance 0.65) (* 200 (dr/gaussian 0 1)))
           :orbit
           [last-orbit
            (if (dr/chance 0.35)
              (gv/vec2 (* (cq/rel-h 0.08) (dr/gaussian 0 1)) (* 50 (dr/gaussian 0 1)))
              (gv/vec2))])))

(defn setup []
  ;; Performance, removes calls to addType & friends
  ;; now dominated by MinorGC and cost of sort?
  (set! (.-disableFriendlyErrors js/p5) true)

  (q/noise-seed (dr/random-int 100000))

  (let [start (random-target)
        factor (/ (+ (q/width) (q/height)) 800)
        brushes (repeatedly (int (* 48 factor))
                            #(make-stroke (g/random-point-inside start)
                                          (g/random-point-inside start)))]
    (-> {:image (q/create-graphics (q/width) (q/height))
         :current start
         :target start
         :factor factor
         :cohorts 12
         :brushes brushes
         :orbit [(gv/vec2) (gv/vec2)]}
        (transition-to 0 (random-target)))))

(defn debug-filter [state]
  (-> state
      (update :brushes count)
      (assoc :transition
             (let [{:keys [base interval]} (:transition state)]
               [base interval]))
      (dissoc :image)
      (dissoc :brush-cohorts)))

(defn update-state [{:keys [transition] :as state}]
  (let [fc (q/frame-count)]
    (if (transition/complete? transition fc)
      (let [state' (transition-to state fc (random-target))]
        (reset! defo (debug-filter state'))
        state')
      state)))

(defn map-noise [t rate offset interval]
  (tm/map-interval (q/noise (/ t rate) offset) [0 1] interval))

(defn draw
  [{:keys [image current target transition factor brush-cohorts variance spin] :as state}]

  ;; measure/beat
  (let [frame-count (q/frame-count)
        tween (var-rate (transition/percent transition frame-count))
        orbit (orbit-transition state)]
    (q/with-graphics image
      (q/color-mode :hsl 1.0)
      ;; Calculate stroke, stroke-weight, and fill for all brushes in a cohort
      (doseq [cohort brush-cohorts
              :let [fc (+ frame-count (* (:cohort (first cohort))
                                         (second variance)))
                    scale (* factor (tm/mix-exp 1.0 32 (q/noise (/ fc 500) 4000.0) 12))]]
        (q/stroke 0 0
                  (tm/smoothstep* 0.45 0.7 (q/noise (/ fc 550) 5000.0))
                  (map-noise fc 650 6000.0 [0.2 0.6]))
        (q/stroke-weight (* 0.6 (tm/smoothstep* 0.35 1.0 (q/noise (/ fc 600) 0.0))))
        (q/fill (mod (* 3 (q/noise (/ fc 3000) 200.0)) 1.0)
                (map-noise fc 800 500.00 [0.4 1.0])
                (map-noise fc 800 1000.0 [0.45 1.0])
                (map-noise fc 500 2000.0 [0.001 0.040]))
        ;; Draw each brush in the cohort
        (draw-brush-cohort cohort scale orbit tween spin))))

  (q/color-mode :hsl 1.0)
  (q/background 1.0)
  (q/image image 0 0)
  (when (:debug @ui-state)
    (q/no-fill)
    (q/stroke-weight 1)
    (q/stroke 0 1.0 1.0 1.0)
    (cq/draw-polygon current)
    (q/stroke 0 0.0 0.0 1.0)
    (cq/draw-polygon target)))

(defn ui-controls []
  [:div
   (ctrl/checkbox ui-state "Debug" [:debug])
   (when (:debug @ui-state)
     (debug/display defo))])

(sketch/defquil superposition
  :created-at "2021-03-08"
  :on-mount (fn [] (ctrl/mount ui-controls))
  :size [1200 900]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
