(ns shimmers.sketches.phase-loop
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.string :as scs]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [[h v] (dr/weighted {[true true] 1.0
                            [true false] 1.0
                            [false true] 1.0})]
    {:horizontal h
     :vertical v
     :r1 (dr/random 0.1 1.0)
     :r2 (dr/random 0.1 1.0)
     :p1 (dr/random)
     :p2 (dr/random)}))

(defn update-state [state]
  state)

(defn f1 [x t]
  (eq/unit-sin (* eq/TAU (+ x (* 0.1 t)
                       (* 0.2 (math/cos (+ x (* 0.4 t))))))))

(defn f2 [x t]
  (eq/unit-cos (* eq/TAU (+ x (* 0.2 t)
                       (math/sin (+ x (* 0.55 t)))))))

(defn rotate [p r]
  (let [c (cq/rel-vec 0.5 0.5)]
    (tm/+ c (g/rotate (tm/- p c) r))))

;; TODO: more rotation or axis-rotation?
(defn draw [{:keys [horizontal vertical r1 r2 p1 p2]}]
  (q/background 1.0)
  (let [t (* 0.001 (q/millis))
        rx (tm/smoothstep* 0.95 1.0 (eq/unit-sin (+ (* 0.00011 t) (* 0.5 p1))))
        ry (tm/smoothstep* 0.95 1.0 (eq/unit-sin (+ (* 0.00013 t) (* 0.5 p2))))]
    (q/fill 0.0)
    (q/text (scs/cl-format "~0,6f ~0,6f" rx ry) 5 10)
    (q/no-fill)
    (when horizontal
      (doseq [x (tm/norm-range 80)]
        (q/line (rotate (cq/rel-vec x (f1 (+ (* r1 x) p1) t)) (+ rx p2))
                (rotate (cq/rel-vec x (f2 (+ (* r2 x) p2) t)) (+ ry p1)))))
    (when vertical
      (doseq [y (tm/norm-range 80)]
        (q/line (rotate (cq/rel-vec (f2 (+ (* r1 y) p1) t) y) (- p1 rx))
                (rotate (cq/rel-vec (f1 (+ (* r2 y) p2) t) y) (- p2 ry)))))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition phase-loop
  {:created-at "2026-01-31"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
