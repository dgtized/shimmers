(ns shimmers.sketches.rolling-shapes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/frame-rate 4)
  {:t 0
   :shapes [(gl/line2 (cq/rel-pos 0.1 0.2) (cq/rel-pos 0.2 0.2))]})

(defn rotate [t shape]
  (let [vertices (geom/vertices shape)
        cycle (mod (q/floor (/ t Math/PI)) 8)
        idx (Math/floor (mod (inc (/ cycle 2)) (count vertices)))
        vertex (nth vertices idx)
        offset (gv/vec2 (* cycle (geom/width shape)) 0)]
    (println [t :cycle cycle :idx idx :vertex vertex])
    (-> shape
        ;; (geom/center vertex)
        (geom/translate (tm/- vertex))
        (geom/rotate t)
        (geom/translate vertex)
        (geom/translate offset))))

(defn update-state [{:keys [t] :as state}]
  (-> state (update :t + 0.2)))

(defn draw [{:keys [shapes t]}]
  (q/no-fill)
  (doseq [s (map (partial rotate t) shapes)]
    (q/begin-shape)
    (doseq [[x y] (geom/vertices s)]
      (q/vertex x y))
    (q/end-shape)))

(sketch/defquil rolling-shapes
  :created-at "2021-06-30"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
