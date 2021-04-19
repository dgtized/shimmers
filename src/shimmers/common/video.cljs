(ns shimmers.common.video
  (:require quil.sketch))

(defn capture [width height]
  (let [capture (.createCapture (quil.sketch/current-applet) "video")]
    (.size capture width height)
    (.hide capture)
    capture))

