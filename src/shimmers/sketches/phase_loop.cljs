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
   [shimmers.math.stair :as ms]
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
  (eq/unit-sin (* eq/TAU (+ x (* 0.11 t)
                       (* 0.3 (math/cos (+ x (* 0.31 t))))))))

(defn f2 [x t]
  (eq/unit-cos (* eq/TAU (+ x (* 0.17 t)
                       (* 0.7 (math/sin (+ x (* 0.37 t))))))))

(defn rotate [p r]
  (let [c (cq/rel-vec 0.5 0.5)]
    (tm/+ c (g/rotate (tm/- p c) r))))

(defn bismooth [e0 e1 x]
  (cond (< (abs x) e0) 0.0
        (> (abs x) e1) 1.0
        :else
        (* (tm/sign x) (tm/smoothstep* e0 e1 (abs x)))))

(comment (for [x (range -2 2 0.1)]
           [x (bismooth 0.5 1.0 x)]))

(defn spacing [t]
  (mapv (fn [s]
          (let [k (math/sin (+ (* 0.11 t) (math/cos (+ (* 0.17 t) s))))
                offset (bismooth 0.6 1.0 (math/sin (+ 1.1 (* 0.13 t) (* 0.2 (math/sin (+ 0.5 t))))))]
            (mod (+ offset (ms/staircase (* 2.0 (bismooth 0.6 1.0 k)) s)) 1.0)))
        (tm/norm-range 80)))

;; FIXME: need to reproject x onto the diagonal lines during rotation
;; so that x stretches to account for crossing on hypotenuse instead of a
;; straight line from one edge of rectangle to opposite.
;; without this, during rotation the first and last lines start inside of the
;; rectangle instead of at edges.
(defn draw [{:keys [horizontal vertical r1 r2 p1 p2]}]
  (q/background 1.0)
  (let [t (* 0.001 (q/millis))
        rot1 (* eq/TAU (tm/smoothstep* 0.2 1.0
                                  (eq/unit-sin (+ (* 0.00011 t)
                                                  (eq/unit-sin (+ (* 0.001 t) (* 0.5 p1)))))))
        rot2 (* eq/TAU (tm/smoothstep* 0.2 1.0
                                  (eq/unit-sin (+ (* 0.00007 t)
                                                  (eq/unit-sin (+ (* 0.001 t) (* 0.5 p2)))))))
        spaces (spacing t)]
    (q/fill 0.0)
    (q/text (scs/cl-format "~0,6f ~0,6f" rot1 rot2) 5 10)
    (q/no-fill)
    (when horizontal
      (doseq [x spaces]
        (q/line (rotate (cq/rel-vec x (f1 (+ (* r1 x) p1) t)) rot1)
                (rotate (cq/rel-vec x (f2 (+ (* r2 x) p2) t)) rot1))))
    (when vertical
      (doseq [y spaces]
        (q/line (rotate (cq/rel-vec (f2 (+ (* r1 y) p1) t) y) rot2)
                (rotate (cq/rel-vec (f1 (+ (* r2 y) p2) t) y) rot2))))))

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
