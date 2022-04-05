(ns shimmers.sketches.pulsing-grid
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.utils.subdiv :as gsd]
   [thi.ng.math.core :as tm]))

(defn rounding [rect]
  (as-> rect _
    (g/as-polygon _)
    (g/sample-uniform _ (/ (g/circumference _) 8) false)
    (gsd/subdivide-closed (:chaikin gsd/schemes) _)
    (gsd/subdivide-closed (:chaikin gsd/schemes) _)
    (gp/polygon2 _)))

(comment
  ;; sample-uniform /6 or other multiples results in a teardrop not clear if
  ;; subdivide-close is handling first/last point correctly or some other issue
  (rounding (rect/rect 0 0 1 1)))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [r (rect/rect (cq/rel-w 0.05) (cq/rel-h 0.35)
                     (cq/rel-w 0.90) (cq/rel-h 0.30))
        cells (g/subdivide r {:rows 6 :cols 24})]
    {:cells (for [cell cells]
              (assoc (rounding (g/scale-size cell 0.9))
                     :color [0 1.0]
                     :pulse [(tm/random 0.1 0.9) (tm/random 0 10)]))
     :t 0}))

;; TODO: use a step function to split between two sin functions with a smaller
;; period for reversal and a longer period for positive motion.
(defn delta-time
  "Make time flow like a sin-function, occasionally backwards"
  [rate scale offset]
  (-> (q/frame-count)
      (* rate)
      Math/sin
      (* scale)
      (+ offset)))

(defn color-cell [t {[period phase] :pulse :as cell}]
  (let [x (/ (-> cell :points first :x) (q/width))
        color
        (-> t
            (+ phase)
            (* period)
            Math/cos
            (tm/map-interval-clamped [-1 1] [1 0])
            (* (- 1.0 (eq/gaussian 0.4 (- (mod (* t 0.2) 2.0) 0.5) 0.18 x))))]
    (assoc cell :color [color 1.0])))

(defn update-state [{:keys [cells t] :as state}]
  (assoc state
         :cells (map (partial color-cell t) cells)
         :t (+ t (delta-time (/ 1 270) 0.03 0.02))))

(defn draw [{:keys [cells]}]
  (q/background 1.0)
  (q/no-stroke)
  (doseq [{:keys [color] :as cell} cells]
    (apply q/fill color)
    (cq/draw-polygon cell)))

(sketch/defquil pulsing-grid
  :created-at "2021-08-21"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
