(ns shimmers.sketches.superposition
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq :refer [rel-h rel-w]]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.geometry :as geometry]
            [shimmers.math.probability :as p]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defonce ui-state (ctrl/state {:debug false}))

(defn explanation []
  [:div
   (ctrl/checkbox ui-state "Debug" [:debug])])

(defn draw-polygon [poly]
  (cq/draw-shape (geom/vertices poly)))

(defn random-shape-at [[p1 p2] t spin [radius freq] scale]
  (-> (gt/triangle2 [0 0] [0 13] [17 0])
      (geom/scale-size scale)
      (geom/rotate (if spin
                     (* spin t)
                     (* 2 Math/PI (rand))))
      (geom/translate (gv/vec2 radius 0))
      (geom/rotate (* t freq))
      (geom/translate (tm/mix p1 p2 t))))

(defn random-triangle []
  (let [s (q/random 0.1 0.2)]
    (-> (gt/triangle2 [0 0] [0 (rel-h s)] [(rel-w s) 0])
        (geometry/rotate-around-centroid (* 2 Math/PI (rand)))
        (geom/translate (cq/rel-pos (q/random 0.2 0.8)
                                    (q/random 0.2 0.8))))))

(defn random-rect []
  (let [w (q/random 0.1 0.3)
        h (q/random 0.1 0.3)]
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
  (q/color-mode :hsl 1.0)
  (let [current (random-target)
        target (random-target)
        factor (/ (+ (q/width) (q/height)) 800)]
    {:current current
     :target target
     :factor factor
     :brushes (repeatedly (int (* 64 factor))
                          (fn [] [(geom/random-point-inside current)
                                 (geom/random-point-inside target)]))
     :base 0
     :spin nil
     :orbit [(gv/vec2) (gv/vec2)]
     :interval 500}))

(defn orbit-transition
  "Transition from old orbit to new in the first 20% of the motion from A to B.

  Reduces discontinuities when brush moves into a new orbit."
  [{:keys [orbit tween]}]
  (tm/mix (first orbit) (second orbit) (tm/smoothstep* 0 0.2 tween)))

(defn transition-to [state fc target]
  (assoc state :current (:target state)
         :target target
         :brushes (map (fn [b] [(second b) (geom/random-point-inside target)])
                       (:brushes state))
         :base fc
         :interval (q/floor (q/random 120 600))
         :spin (when (p/chance 0.65) (* 200 (q/random-gaussian)))
         :orbit [(second (:orbit state))
                 (if (p/chance 0.35)
                   (gv/vec2 (* (cq/rel-h 0.08) (q/random-gaussian)) (* 50 (q/random-gaussian)))
                   (gv/vec2))]
         :tween 0.0))

(defn update-state [{:keys [base interval] :as state}]
  (let [fc (q/frame-count)]
    (if (= (- fc base) interval)
      (let [state' (transition-to state fc (random-target))]
        (.log js/console (dissoc state' :brushes))
        state')
      (assoc state :tween (var-rate (/ (- fc base) interval))))))

(defn map-noise [t rate offset interval]
  (tm/map-interval (q/noise (/ t rate) offset) [0 1] interval))

(defn draw
  [{:keys [current target tween factor brushes spin] :as state}]
  (when (:debug @ui-state)
    (q/no-fill)
    (q/stroke-weight 1)
    (q/stroke 0 1.0 1.0 1.0)
    (draw-polygon current)
    (q/stroke 0 0.0 0.0 1.0)
    (draw-polygon target))

  ;; (q/no-stroke)
  ;; measure/beat
  (let [fc (q/frame-count)
        scale (tm/mix-exp 1.0 32 (q/noise (/ fc 500) 4000.0) 12)
        orbit (orbit-transition state)]
    (q/stroke 0 0
              (tm/smoothstep* 0.45 0.7 (q/noise (/ fc 550) 5000.0))
              (map-noise fc 650 6000.0 [0.2 0.6]))
    (q/stroke-weight (* 0.6 (tm/smoothstep* 0.35 1.0 (q/noise (/ fc 600) 0.0))))
    (q/fill (mod (* 3 (q/noise (/ fc 3000) 200.0)) 1.0)
            (map-noise fc 800 500.00 [0.4 1.0])
            (map-noise fc 800 1000.0 [0.45 1.0])
            (map-noise fc 500 2000.0 [0.001 0.040]))
    (doseq [brush brushes]
      (draw-polygon (random-shape-at brush tween spin orbit (* factor scale))))))

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
