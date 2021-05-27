(ns shimmers.sketches.superposition
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq :refer [rel-h rel-w]]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.geometry :as geometry]
            [shimmers.math.probability :as p]
            [thi.ng.geom.bezier :as bezier]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.utils :as gu]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

;; Represent a brush stroke from location p to q
;; caches the resulting spline and index of positions
(defrecord Stroke [p q curve spline arc-length-idx]
  geom/ISample
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

(defn explanation []
  [:div
   (ctrl/checkbox ui-state "Debug" [:debug])])

(defn draw-triangle [{[[ax ay] [bx by] [cx cy]] :points}]
  (q/triangle ax ay bx by cx cy))

(defn draw-polygon [poly]
  (cq/draw-shape (geom/vertices poly)))

(defn brush-at [stroke [radius freq] t]
  (tm/+ (gv/vec2)
        (gv/vec2 radius (* t freq)) ;; rotate around origin/path
        ;; point-at is expensive on splines, can this be precomputed?
        (geom/point-at stroke t)))

(def triangle-instance
  (let [t (gt/triangle2 [0 0] [0 13] [17 0])]
    (assoc t :centroid (gu/centroid (:points t)))))

;; optimized version with cached centroid, also trying to reduce consing?
(defn random-shape-at [position t spin scale]
  (let [{:keys [points centroid]} triangle-instance
        theta (if spin
                (* spin t)
                (* 2 Math/PI (rand)))]
    (apply gt/triangle2
           (mapv (fn [p] (-> p
                            (tm/- centroid)
                            (tm/madd scale centroid)
                            (geom/rotate theta)
                            (tm/+ position)))
                 points))))
(comment
  ;; un-optimized version of random-shape-at?
  (defn random-shape-at [position t spin scale]
    (-> (gt/triangle2 [0 0] [0 13] [17 0])
        (geom/scale-size scale)
        (geom/rotate (if spin
                       (* spin t)
                       (* 2 Math/PI (rand))))
        (geom/translate position))))

(defn random-triangle []
  (let [s (q/random 0.15 0.5)
        r [0.2 0.8]]
    (-> (gt/triangle2 [0 0] [0 (rel-h s)] [(rel-w s) 0])
        (geometry/rotate-around-centroid (* 2 Math/PI (rand)))
        (geom/translate (cq/rel-pos (apply q/random r)
                                    (apply q/random r))))))

(defn random-rect []
  (let [w (q/random 0.15 0.5)
        h (q/random 0.15 0.5)]
    (-> (rect/rect (rel-w (* (- 1 w) (rand))) (rel-h (* (- 1 h) (rand)))
                   (rel-w w) (rel-h h))
        (geometry/rotate-around-centroid (* 2 Math/PI (rand))))))

(defn random-circle []
  (let [r (q/random 0.05 0.35)]
    (gc/circle (cq/rel-pos (tm/clamp (rand) r (- 1 r))
                           (tm/clamp (rand) r (- 1 r)))
               (rel-h r))))

(defn random-target []
  ((rand-nth [random-rect random-circle random-triangle])))

(defn var-rate [n]
  (Math/sin (* (/ Math/PI 2) n)))

(defn setup []
  ;; Performance, removes calls to addType & friends
  ;; now dominated by MinorGC and cost of sort?
  (set! (.-disableFriendlyErrors js/p5) true)

  (let [current (random-target)
        target (random-target)
        factor (/ (+ (q/width) (q/height)) 800)]
    {:image (q/create-graphics (q/width) (q/height))
     :current current
     :target target
     :factor factor
     :brushes (repeatedly (int (* 48 factor))
                          #(make-stroke (geom/random-point-inside current)
                                        (geom/random-point-inside target)))
     :variance [1 0]
     :base 0
     :spin nil
     :orbit [(gv/vec2) (gv/vec2)]
     :interval 500}))

(defn orbit-transition
  "Transition from old orbit to new in the first 20% of the motion from A to B.

  Reduces discontinuities when brush moves into a new orbit."
  [{:keys [orbit tween]}]
  (tm/mix (first orbit) (second orbit) (tm/smoothstep* 0 0.2 tween)))

(defn transition-to
  [{brushes :brushes [_ last-orbit] :orbit previous :target :as state}
   fc target]
  (let [curve (* 0.8 (p/happensity 0.4))
        cohorts 12]
    (assoc state :current previous
           :target target
           :brushes
           (->> brushes
                (map-indexed
                 (fn [idx brush]
                   (let [p (brush-at brush last-orbit 1.0)
                         q (geom/random-point-inside target)]
                     (assoc (make-stroke p q curve)
                            :cohort (mod idx cohorts)))))
                (sort-by :cohort))

           :variance [cohorts (* 20 (q/random-gaussian))]
           :base fc
           :tween 0.0
           :interval (q/floor (q/random 120 600))
           :spin (when (p/chance 0.65) (* 200 (q/random-gaussian)))
           :orbit
           [last-orbit
            (if (p/chance 0.35)
              (gv/vec2 (* (cq/rel-h 0.08) (q/random-gaussian)) (* 50 (q/random-gaussian)))
              (gv/vec2))])))

(defn update-state [{:keys [base interval] :as state}]
  (let [fc (q/frame-count)]
    (if (= (- fc base) interval)
      (let [state' (transition-to state fc (random-target))]
        (.log js/console (dissoc (update state' :brushes count) :image))
        state')
      (assoc state :tween (var-rate (/ (- fc base) interval))))))

(defn map-noise [t rate offset interval]
  (tm/map-interval (q/noise (/ t rate) offset) [0 1] interval))

(defn draw
  [{:keys [image current target tween factor brushes variance spin] :as state}]

  ;; measure/beat
  (let [frame-count (q/frame-count)
        orbit (orbit-transition state)]
    (q/with-graphics image
      (q/color-mode :hsl 1.0)
      ;; Calculate stroke, stroke-weight, and fill for all brushes in a cohort
      (doseq [cohort (partition-by :cohort brushes)
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
        (doseq [brush cohort
                :let [position (brush-at brush orbit tween)]]
          (draw-triangle (random-shape-at position tween spin scale))))))

  (q/color-mode :hsl 1.0)
  (q/background 1.0)
  (q/image image 0 0)
  (when (:debug @ui-state)
    (q/no-fill)
    (q/stroke-weight 1)
    (q/stroke 0 1.0 1.0 1.0)
    (draw-polygon current)
    (q/stroke 0 0.0 0.0 1.0)
    (draw-polygon target)))

(defn ^:export run-sketch []
  ;; 20210308
  (ctrl/mount explanation)
  (q/defsketch superposition
    :host "quil-host"
    :size [1200 900]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
