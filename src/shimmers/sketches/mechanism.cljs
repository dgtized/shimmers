(ns shimmers.sketches.mechanism
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.polygon :as gp]
            [thi.ng.geom.vector :as gv]
            [thi.ng.geom.core :as geom]
            [shimmers.common.quil :as cq]
            [thi.ng.math.core :as tm]))

;; http://www.gearseds.com/files/Approx_method_draw_involute_tooth_rev2.pdf
(defn tooth [height width p]
  (let [polar (geom/as-polar p)
        hw (/ width 2)
        qw (/ width 3)
        pitch (* height 0.66)]
    (mapv (fn [t] (geom/as-cartesian (tm/+ polar (gv/vec2 t))))
          [[0 (- width)] [pitch (- hw)] [height (- qw)]
           [height qw] [pitch hw] [0 width]])))

(defn gear [radius teeth]
  (let [points (-> (gc/circle (gv/vec2) radius)
                   (geom/vertices teeth))]
    (gp/polygon2 (mapcat (partial tooth (* radius 0.3) (/ tm/TWO_PI teeth 3)) points))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0})

(defn update-state [state]
  (update state :t + 0.001))

(defn draw [{:keys [t]}]
  (q/background 1.0)
  (cq/draw-shape (geom/vertices (-> (gear 50 8)
                                    (geom/rotate t)
                                    (geom/translate (gv/vec2 (cq/rel-pos 0.5 0.5)))))))

(defn ^:export run-sketch []
  ;; 20210419
  (q/defsketch mechanism
    :host "quil-host"
    :size [800 800]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
