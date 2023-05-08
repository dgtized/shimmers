(ns shimmers.sketches.rolling-shapes
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; Concept here is to "roll" a particle polygon along a line
;; Related to https://en.wikipedia.org/wiki/Cyclogon
;; Currently only playing with rolling a single line

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/frame-rate 30)
  {:t 0
   :shapes [(gl/line2 (cq/rel-pos 0.1 0.2) (cq/rel-pos 0.2 0.2))]})

;; FIXME: Works for a line on the first pass, but goes below for second
(defn rotate [t shape]
  (let [vertices (g/vertices shape)
        cycle (mod (q/floor (/ t Math/PI)) 9)
        idx (mod (inc cycle) (count vertices))
        vertex (nth vertices idx)
        offset (gv/vec2 (* (- cycle idx) (g/width shape)) 0)]
    (-> shape
        (g/translate (tm/- vertex))
        (g/rotate t)
        (g/translate vertex)
        (g/translate offset))))

(defn update-state [state]
  (-> state (update :t + 0.05)))

(defn draw [{:keys [shapes t]}]
  (q/stroke-weight 0.25)
  (q/no-fill)
  (doseq [s (map (partial rotate t) shapes)]
    (cq/draw-path (g/vertices s))))

(defn page []
  (sketch/component
   :size [800 600]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition rolling-shapes
  {:created-at "2021-06-30"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
