(ns shimmers.sketches.phase-loop
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [[h v] (dr/weighted {[true true] 1.0
                            [true false] 1.0
                            [false true] 1.0})]
    {:horizontal h
     :vertical v}))

(defn update-state [state]
  state)

(defn f1 [x t]
  (eq/unit-sin (* eq/TAU (+ x (* 0.1 t)
                       (* 0.2 (math/cos (+ x t)))))))

(defn f2 [x t]
  (eq/unit-cos (* eq/TAU (+ x (* 0.3 t)
                       (math/sin (+ x t))))))

;; TODO: rotation?
(defn draw [{:keys [horizontal vertical]}]
  (q/background 1.0)
  (let [t (* 0.001 (q/millis))]
    (when horizontal
      (doseq [x (tm/norm-range 90)]
        (q/line (cq/rel-vec x (f1 x t))
                (cq/rel-vec x (f2 x t)))))
    (when vertical
      (doseq [y (tm/norm-range 90)]
        (q/line (cq/rel-vec (f2 y t) y)
                (cq/rel-vec (f1 y t) y))))))

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
