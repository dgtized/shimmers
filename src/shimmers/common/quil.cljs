(ns shimmers.common.quil
  (:require [quil.core :as q :include-macros true]))

(defn lerp-line [[x y] [x' y'] amt]
  (q/line x y (q/lerp x x' amt) (q/lerp y y' amt)))
