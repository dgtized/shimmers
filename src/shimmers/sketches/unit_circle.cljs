(ns shimmers.sketches.unit-circle
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.vector :as gv]
            [thi.ng.geom.core :as g]
            [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:radius (cq/rel-h 0.40)})

(defn update-state [state]
  state)

(defn draw-unit [radius]
  (q/stroke-weight 1.0)
  (q/no-fill)
  (cq/circle [0 0] radius))

(defn draw-bisector [{[p q] :points} weight]
  (q/stroke-weight weight)
  (q/line p q)
  (q/text-size 16)
  (q/fill 0)
  (let [theta (g/heading q)
        num (tm/roundto theta 0.01)
        [x0 y0] (-> q
                    (g/scale 1.1)
                    (g/translate (gv/vec2 (* -0.5 (q/text-width num)) 6)))]
    (q/text-num num x0 y0)))

(defn draw [{:keys [radius]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/translate (cq/rel-pos 0.5 0.5))
  (draw-unit radius)
  (let [axis [(gl/line2 (gv/vec2) [0 radius])
              (gl/line2 (gv/vec2) [0 (- radius)])
              (gl/line2 (gv/vec2) [radius 0])
              (gl/line2 (gv/vec2) [(- radius) 0])]
        quarter-axis (map #(g/rotate % (- (/ Math/PI 4))) axis)]
    (doseq [line axis]
      (draw-bisector line 0.5))
    (doseq [line quarter-axis]
      (draw-bisector line 0.3)))
  )

(sketch/defquil unit-circle
  :created-at "2021-10-28"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
