(ns shimmers.common.shader
  (:require [quil.core :as q :include-macros true]))

#_{:clj-kondo/ignore [:unresolved-var]}
(defn pass [shader [w h] uniforms]
  (q/shader shader)
  (doseq [[key value] uniforms]
    (q/set-uniform shader key value))
  (q/rect (* -0.5 w) (* -0.5 h) w h))

(defn transform
  [shader buffer image [w h] uniforms]
  (when (q/loaded? shader)
    (q/with-graphics buffer
      (pass shader [w h] uniforms))
    (q/with-graphics image
      (q/image buffer 0 0 w h))))
