(ns shimmers.sketches.stem-and-leaf
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:circles [(gc/circle (cq/rel-pos 0.5 0.5) (cq/rel-h 0.1))
             (assoc (gc/circle (cq/rel-pos 0.2 0.5) (cq/rel-h 0.15)) :parent 0)
             (assoc (gc/circle (cq/rel-pos 0.8 0.5) (cq/rel-h 0.05)) :parent 0)
             (assoc (gc/circle (cq/rel-pos 0.5 0.2) (cq/rel-h 0.15)) :parent 0)
             (assoc (gc/circle (cq/rel-pos 0.5 0.8) (cq/rel-h 0.05)) :parent 0)]})

(defn update-state [state]
  state)

(defn curve-by [points]
  (q/begin-shape)
  (doseq [[x y] points]
    (q/curve-vertex x y))
  (q/end-shape))

(defn tangent-lines [c1 c2]
  (let [{:keys [p r]} c1
        {p' :p r' :r} c2
        angle (+ (* 0.5 Math/PI) (geom/heading (tm/- p p')))]
    (q/line (tm/+ p (v/polar r angle)) (tm/+ p' (v/polar r' angle)))
    (q/line (tm/- p (v/polar r angle)) (tm/- p' (v/polar r' angle)))))

(defn draw [{:keys [circles]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  (doseq [{:keys [p r]} circles]
    (cq/circle p r))
  (doseq [{:keys [parent] :as c1} circles
          :when parent
          :let [c2 (nth circles parent)]]
    (tangent-lines c1 c2)

    #_(curve-by [(tm/+ p (v/polar r (* 1.5 Math/PI)))
                 (tm/+ p (v/polar r (* 1.6 Math/PI)))
                 (tm/+ p (v/polar r (* 1.7 Math/PI)))
                 (tm/+ p' (tm/* (tm/- p p') 0.5)) ;; midpoint
                 (tm/+ p' (v/polar r' (* 0.50 Math/PI)))
                 (tm/+ p' (v/polar r' (* 0.33 Math/PI)))])
    ))

(sketch/defquil stem-and-leaf
  :created-at "2021-07-21"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
