(ns shimmers.sketches.reaction-diffusion
  "From https://www.karlsims.com/rd.html and https://ciphrd.com/2019/08/24/reaction-diffusion-on-shader/."
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]))

(defn setup []
  (q/color-mode :rgb 1.0)
  (let [[width height] [256 256]
        initial-image (q/create-graphics width height :p2d)]
    (q/with-graphics initial-image
      (q/color-mode :rgb 1.0)
      (q/background 0.0 1.0 0.0 1.0)
      (q/no-stroke)
      (q/fill 1.0 1.0 0.0 1.0)
      (q/ellipse (/ width 2) (/ height 2) 32 32))
    {:image-size [width height]
     :in-buffer initial-image
     :out-buffer (q/create-graphics width height :p3d)
     :shader (q/load-shader "shaders/reaction-diffusion.frag.c"
                            "shaders/reaction-diffusion.vert.c")}))


;; Cribbed some of the feedback loop from https://medium.com/@edoueda/integrating-p5-js-and-webgl-with-react-js-96c848a63170
(defn update-state [{:keys [image-size shader in-buffer out-buffer] :as state}]
  (let [[w h] image-size]
    (when (q/loaded? shader)
      (q/with-graphics out-buffer
        (q/shader shader)
        (q/set-uniform shader "resolution" (array w h))
        (q/set-uniform shader "concentrations" in-buffer)
        (q/set-uniform shader "diffusionA" 1.0)
        (q/set-uniform shader "diffusionB" 0.2)
        (q/set-uniform shader "feed" 0.05)
        (q/set-uniform shader "kill" 0.02)
        (q/set-uniform shader "deltaT" 0.1)
        (q/rect (* -0.5 w) (* -0.5 h) w h))
      (q/with-graphics in-buffer
        (q/image out-buffer 0 0 w h)))
    state))

(defn draw [{:keys [in-buffer]}]
  (q/image in-buffer 0 0 (q/width) (q/height)))

(sketch/defquil reaction-diffusion
  :created-at "2021-09-15"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
