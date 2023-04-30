(ns shimmers.sketches.video-shader
  "Basic concepts translated from
  https://itp-xstory.github.io/p5js-shaders/#/./docs/examples/image_effects and
  combined with https://thebookofshaders.com/07/.
  "
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.shader :as shader]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.video :as video]
   [shimmers.sketch :as sketch :include-macros true]))

(def modes {:specular-mouse 0
            :edge-detection 1})

(defonce ui-state (ctrl/state {:mode :edge-detection}))

;; HACK: Shaders are renamed to .c because github-pages requires a mime type to
;; serve, per [1], but [2] has no extension for mime-type x-shader/x-fragment, or
;; x-shader/x-vertex.
;; [1] https://docs.github.com/en/github/working-with-github-pages/about-github-pages#mime-types-on-github-pages
;; [2] https://github.com/jshttp/mime-db/blob/93b1c9c90316484c682532384c493c682f4a459f/db.json#L8307-L8312
(defn setup []
  {:camera (video/capture 640 480)
   :shader (q/load-shader "shaders/video-shader.frag.c"
                          "shaders/video-shader.vert.c")})

(defn draw [{:keys [camera shader]}]
  (let [[w h] [(q/width) (q/height)]
        ;; Note quite working, p3d context is supposed to be relative to
        ;; -0.5*w,-0.5*h or something? https://p5js.org/reference/#/p5/mouseX,
        ;; but it's all over the place and also a function of display-density I
        ;; think?
        mouse (array (- (q/mouse-x) w) (- (* 0.5 h) (q/mouse-y)))]
    (when (q/loaded? shader)
      (shader/pass shader [w h]
                   {"u_resolution" (array w h)
                    "u_time" (/ (q/millis) 1000.0)
                    "u_mouse" mouse
                    "videoTexture" camera
                    "u_mode" (get modes (:mode @ui-state))}))))

(defn page []
  [:div
   (sketch/component
    :size [640 480]
    :renderer :p3d
    :setup setup
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:div.contained.explanation
    [ctrl/change-mode ui-state (keys modes)]]])

(sketch/definition video-shader
  {:created-at "2021-02-14"
   :tags #{:camera :shader}
   :type :quil}
  (ctrl/mount page "sketch-host"))
