(ns shimmers.sketches.hex-divisions
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.hexagon :as hex]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.vector :as gv]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:divisions 1})

(defn update-state [state]
  (update state :divisions #(mod (inc %) 24)))

(defn draw [{:keys [divisions]}]
  (let [r (/ (* 0.25 (q/height)) divisions)]
    (doseq [pos (hex/cube-spiral (gv/vec3) divisions)
            :let [hex (hex/hexagon (hex/axial->hex r (hex/cube->axial pos)) r)]]
      (cq/draw-shape (geom/vertices (geom/translate hex (cq/rel-pos 0.5 0.5)) 6)))))

(defn ^:export run-sketch []
  ;; 20210523
  (q/defsketch hex-divisions
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
