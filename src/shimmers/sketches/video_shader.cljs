(ns shimmers.sketches.video-shader
  "Basic concepts translated from
  https://itp-xstory.github.io/p5js-shaders/#/./docs/examples/image_effects and
  combined with https://thebookofshaders.com/07/.
  "
  (:require [goog.dom :as dom]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            quil.sketch
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.ui :as ui]))

(def modes {:specular-mouse 0
            :edge-detection 1})

(defn cycle-mode [state]
  {:mode (ui/cycle-next (keys modes) (:mode state))})

(defonce ui-state (r/atom {:mode :edge-detection}))

(defn interface []
  (let [mode (:mode @ui-state)]
    [:div
     [:input {:type "button" :value "Cycle Mode"
              :on-click #(swap! ui-state cycle-mode)}]
     [:span {:style {:padding-left "1em"}} "Mode: " (name mode)]]))

;; HACK: Shaders are renamed to .c because github-pages requires a mime type to
;; serve, per [1], but [2] has no extension for mime-type x-shader/x-fragment, or
;; x-shader/x-vertex.
;; [1] https://docs.github.com/en/github/working-with-github-pages/about-github-pages#mime-types-on-github-pages
;; [2] https://github.com/jshttp/mime-db/blob/93b1c9c90316484c682532384c493c682f4a459f/db.json#L8307-L8312
(defn setup []
  (let [camera (.createCapture (quil.sketch/current-applet) "video")
        shader (q/load-shader "shaders/video-shader.frag.c"
                              "shaders/video-shader.vert.c")]
    (.size camera 640 480)
    (.hide camera)
    {:camera camera
     :shader shader}))

(defn draw [{:keys [camera shader]}]
  (let [[w h] [(q/width) (q/height)]
        ;; Note quite working, p3d context is supposed to be relative to
        ;; -0.5*w,-0.5*h or something? https://p5js.org/reference/#/p5/mouseX,
        ;; but it's all over the place and also a function of display-density I
        ;; think?
        mouse (array (- (q/mouse-x) w) (- (* 0.5 h) (q/mouse-y)))]
    (when (q/loaded? shader)
      (q/shader shader)
      (q/set-uniform shader "u_resolution" (array w h))
      (q/set-uniform shader "u_time" (/ (q/millis) 1000.0))
      (q/set-uniform shader "u_mouse" mouse)
      (q/set-uniform shader "videoTexture" camera)
      (q/set-uniform shader "u_mode" (get modes (:mode @ui-state)))
      (q/rect 0 0 w h))))

(defn ^:export run-sketch []
  (rdom/render [interface] (dom/getElement "explanation"))

  (q/defsketch video-shader
    :host "quil-host"
    :size [640 480]
    :renderer :p3d
    :setup setup
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
