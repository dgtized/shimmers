(ns shimmers.sketches.lattice-reflections
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  {:radius (cq/rel-h 0.02)
   :n (repeatedly 5 #(dr/random-int 3 13))})

(defn update-state [state]
  state)

(defn surround [{:keys [p r]} n]
  (let [sv (math/sin (/ tm/PI n))
        r' (/ (* r sv) (- 1 sv))
        circle (gc/circle p (+ r r'))]
    (for [s (butlast (tm/norm-range n))]
      (gc/circle (g/point-at circle s) r'))))

(defn circles [radius xs]
  (loop [circles [(gc/circle radius)] r radius xs xs]
    (if (seq xs)
      (let [additions (surround (gc/circle r) (first xs))]
        (recur
         (concat circles additions)
         (+ r (* 2 (:r (first additions))))
         (rest xs)))
      circles)))

(defn draw [{:keys [radius n]}]
  (q/background 1.0)
  (q/translate (cq/rel-vec 0.5 0.5))
  (doseq [c (circles radius n)]
    (qdg/draw c)))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition lattice-reflections
  {:created-at "2026-05-16"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
