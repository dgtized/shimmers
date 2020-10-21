(ns shimmers.core
  (:require [quil.core :as q :include-macros true]
            [shimmers.ray-marching :as ray-marching]))

(enable-console-print!)

;; initialize sketch on first-load
(defonce start-up (ray-marching/run-sketch))

