(ns shimmers.common.shader
  (:require [quil.core :as q :include-macros true]))

(defn pass [shader [w h] uniforms]
  (q/shader shader)
  (doseq [[key value] uniforms]
    (q/set-uniform shader key value))
  (q/rect (* -0.5 w) (* -0.5 h) w h))
