(ns shimmers.sketches.decaying-foundations
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn setup []
  (q/no-loop)
  (q/color-mode :hsl 1.0)
  {})

(defn brick [w h]
  (rect/rect 0 0 w h))

(defn layer [width height y]
  (for [x (range (* (rand) (- width)) (q/width) width)]
    (geom/translate (brick (- width (* 10 (rand))) height)
                    (gv/vec2 x y))))

(defn wall [height]
  (mapcat identity
          (for [y (range 0 (q/height) height)]
            (layer (rand-nth [30 50 60 75 80])
                   (- height (tm/random 4))
                   (+ y (tm/random 4))))))

(defn update-state [state]
  state)

(defn draw [_]
  (doseq [{[x y] :p  [w h] :size} (wall 20)]
    (q/rect x y w h)))

(defn ^:export run-sketch []
  ;; 20210414
  (q/defsketch decaying-foundations
    :host "quil-host"
    :size [900 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
