(ns shimmers.common.video
  (:require [quil.sketch]))

;; TODO: use faceMode constraints to add controls to flip between "user"
;; and "environment" for mobile use. See documentation @
;; https://developer.mozilla.org/en-US/docs/Web/API/MediaDevices/getUserMedia
;; https://p5js.org/reference/#/p5/createCapture
(defn capture [width height]
  (let [capture (.createCapture (quil.sketch/current-applet) "video")]
    (.size capture width height)
    (.hide capture)
    capture))

