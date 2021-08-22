(ns shimmers.sketches.pulsing-grid
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.polygon :as gp]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.utils.subdiv :as gsd]
            [thi.ng.math.core :as tm]))

(defn rounding [rect]
  (as-> rect _
    (geom/as-polygon _)
    (geom/sample-uniform _ (/ (geom/circumference _) 8) false)
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
        cells (geom/subdivide r {:rows 6 :cols 24})]
    {:cells (for [cell cells]
              (assoc (rounding (geom/scale-size cell 0.9))
                     :color [0 1.0]
                     :pulse [(tm/random 0.1 0.9) (tm/random 0 10)]))
     :t 0}))

(defn delta-time
  "Make time flow like a sin-function, occasionally backwards"
  [scale offset]
  (-> (q/frame-count)
      (/ 360)
      Math/sin
      (* scale)
      (+ offset)))

(defn gaussian
  "Bell curve of magnitude `a`, centered at `b`, width `c`.
  From https://en.wikipedia.org/wiki/Gaussian_function"
  [a b c x]
  (* a (Math/exp (- (/ (Math/pow (- x b) 2)
                       (* 2 (* c c)))))))

(comment
  (map (fn [x] [x (gaussian 1.0 0.5 0.2 x)]) (range 0 1 0.05)))

(defn color-cell [t {[period phase] :pulse :as cell}]
  (let [x (/ (-> cell :points first :x) (q/width))
        color
        (-> t
            (+ phase)
            (* period)
            Math/cos
            (+ (gaussian 1.0 (- (mod (* t 0.3) 2.0) 0.5) 0.3 x))
            (tm/map-interval-clamped [-1 1] [0 1]))]
    (assoc cell :color [color 1.0])))

(defn update-state [{:keys [cells t] :as state}]
  (assoc state
         :cells (map (partial color-cell t) cells)
         :t (+ t (delta-time 0.025 0.02))))

(defn draw [{:keys [cells]}]
  (q/background 1.0)
  (q/no-stroke)
  (doseq [{:keys [color] :as cell} cells]
    (apply q/fill color)
    (cq/draw-shape (geom/vertices cell))))

(sketch/defquil pulsing-grid
  :created-at "2021-08-21"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
