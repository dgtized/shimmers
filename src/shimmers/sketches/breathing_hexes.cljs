(ns shimmers.sketches.breathing-hexes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.hexagon :as hex]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

;; Is there a way to make this smoother and stutter less? Also, the transition
;; from a single cell to multiple is a little jarring, maybe someway to smooth
;; that?
(defn draw [_]
  (q/background 1.0 0.025)
  (q/stroke-weight 0.5)
  (q/stroke 0.0 1.0)
  (q/fill 1.0 0.1)
  (let [t (/ (q/millis) 1000)
        divisions (int (tm/map-interval (q/sin t) [-1 1] [0 20]))
        r (/ (* (/ (Math/sqrt 2) 5) (q/height)) (inc divisions))]
    (q/with-translation (cq/rel-pos 0.5 0.5)
      (q/with-rotation [(/ t 12)]
        (doseq [pos (hex/cube-spiral (gv/vec3) divisions)
                :let [hex (hex/hexagon (hex/axial->hex r (hex/cube->axial pos)) r)]]
          (cq/draw-shape (geom/vertices hex 6)))))))

(defn ^:export run-sketch []
  ;; 20210523
  (q/defsketch breathing-hexes
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
