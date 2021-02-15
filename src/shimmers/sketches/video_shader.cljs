(ns shimmers.sketches.video-shader
  "Basic concepts Translated from
  https://itp-xstory.github.io/p5js-shaders/#/./docs/examples/image_effects
  "
  (:require [quil.core :as q :include-macros true]
            quil.sketch
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]))

(defn setup []
  (let [camera (.createCapture (quil.sketch/current-applet) "video")
        shader (q/load-shader "shaders/video-shader.frag"
                              "shaders/video-shader.vert")]
    (.size camera 640 480)
    (.hide camera)
    {:camera camera
     :shader shader}))

(defn draw [{:keys [camera shader]}]
  (let [[w h] [(q/width) (q/height)]]
    (when (q/loaded? shader)
      (q/shader shader)
      (q/set-uniform shader "u_resolution" [w h])
      (q/set-uniform shader "u_time" (/ (q/millis) 1000.0))
      (q/set-uniform shader "u_mouse" [(q/mouse-x) (q/map-range (q/mouse-y) 0 h h 0)])
      (q/set-uniform shader "videoTexture" camera)
      (q/rect 0 0 w h))))

(defn ^:export run-sketch []
  (q/defsketch video-shader
    :host "quil-host"
    :size [600 400]
    :renderer :p3d
    :setup setup
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
