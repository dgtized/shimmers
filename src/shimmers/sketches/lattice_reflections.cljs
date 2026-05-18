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
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  {:radius (cq/rel-h 0.02)
   :n (repeatedly 6 #(dr/random-int 3 13))
   :p (repeatedly 6 #(* (dr/random-sign 0.6)
                        (tm/clamp (dr/gaussian 1 0.4) 0.05 1.95)))})

(defn update-state [state]
  state)

(defn surround [{:keys [p r]} n phase t]
  (let [sv (math/sin (/ tm/PI n))
        r' (/ (* r sv) (- 1 sv))
        circle (gc/circle p (+ r r'))]
    (for [s (butlast (tm/norm-range n))]
      (gc/circle (g/point-at circle (+ s (* phase t))) r'))))

(defn circles [radius xs ps t]
  (loop [circles [(gc/circle radius)] r radius xs xs ps ps]
    (if (seq xs)
      (let [additions (surround (gc/circle r) (first xs) (first ps) t)]
        (recur
         (concat circles additions)
         (+ r (* 2 (:r (first additions))))
         (rest xs)
         (rest ps)))
      circles)))

(defn features [circles radius t]
  (let [s (* 0.11 (math/sin t))]
    (mapcat
     (fn [c]
       (if (< (:r c) (* 1.1 radius))
         [(g/scale-size c (+ 0.5 s))]
         [(g/scale-size c (+ 0.66 s))
          (g/scale-size c (- 0.33 s))]))
     circles)))

(defn draw [{:keys [radius n p]}]
  (q/background 1.0)
  (q/stroke-weight 2.0)
  (q/translate (cq/rel-vec 0.5 0.5))
  (let [t (/ (q/millis) 3000.0)
        radius' (* radius (+ 1 (* 0.5 (eq/unit-sin t))))
        circles (circles radius' n p (/ t 10.0))]
    (doseq [obj (concat circles (features circles radius' t))]
      (qdg/draw obj))))

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
