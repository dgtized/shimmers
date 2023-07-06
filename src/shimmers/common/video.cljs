(ns shimmers.common.video
  (:require [quil.core :as q :include-macros true]
            [quil.sketch]))

;; TODO: use faceMode constraints to add controls to flip between "user"
;; and "environment" for mobile use. See documentation @
;; https://developer.mozilla.org/en-US/docs/Web/API/MediaDevices/getUserMedia
;; https://p5js.org/reference/#/p5/createCapture
(defn capture [width height]
  (let [capture (.createCapture (quil.sketch/current-applet) "video")]
    (.size capture width height)
    (.hide capture)
    capture))

(defn copy-frame [dest capture width height]
  (if capture
    (do (q/copy capture dest
                [0 0 width height]
                [0 0 width height])
        dest)
    dest))
