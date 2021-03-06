(ns shimmers.math.geometry
  (:require [thi.ng.geom.core :as geom]))

(defn rotate-around-centroid [polygon t]
  (-> polygon
      geom/center
      (geom/rotate t)
      (geom/translate (geom/centroid polygon))))

