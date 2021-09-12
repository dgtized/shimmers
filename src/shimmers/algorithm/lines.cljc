(ns shimmers.algorithm.lines
  (:require [thi.ng.geom.line :as gl]))

(defn points->lines [points]
  (map gl/line2 (partition 2 1 points)))
