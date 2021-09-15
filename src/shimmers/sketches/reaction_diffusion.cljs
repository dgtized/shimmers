(ns shimmers.sketches.reaction-diffusion
  "From https://www.karlsims.com/rd.html and https://ciphrd.com/2019/08/24/reaction-diffusion-on-shader/."
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [[width height] [128 128]
        image (q/create-image width height)]
    (q/background 1.0 0.0 0.0 0.0)
    (q/no-stroke)
    (q/fill 0.0 1.0 0.0 1.0)
    (q/ellipse (/ (q/width) 2) (/ (q/width) 2) 32 32)
    (q/copy (q/current-graphics) image [0 0 (q/width) (q/height)] [0 0 width height])
    {:image-size [width height]
     :in-buffer image
     :out-buffer (q/create-graphics width height :p3d)
     :shader (q/load-shader "shaders/reaction-diffusion.frag.c"
                            "shaders/reaction-diffusion.vert.c")}))

(defn update-state [{:keys [image-size shader in-buffer out-buffer] :as state}]
  (let [[w h] image-size]
    (when (q/loaded? shader)
      (q/with-graphics out-buffer
        (q/shader shader)
        (q/set-uniform shader "concentrations" in-buffer)
        (q/set-uniform shader "diffusionA" 1.0)
        (q/set-uniform shader "diffusionB" 0.2)
        (q/set-uniform shader "feed" 0.05)
        (q/set-uniform shader "kill" 0.02)
        (q/set-uniform shader "deltaT" 1.0)
        (q/set-uniform shader "texelSize" (array (/ 1.0 w) (/ 1.0 h)))
        (q/rect 0 0 w h))
      (q/copy out-buffer in-buffer [0 0 w h] [0 0 w h]))
    state))

(defn draw [{:keys [in-buffer]}]
  (q/background 1.0)
  (q/image in-buffer 0 0 (q/width) (q/height)))

(sketch/defquil reaction-diffusion
  :created-at "2021-09-15"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
