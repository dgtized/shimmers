(ns shimmers.common.svg
  (:require [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.svg.adapter :as adapt]))

(defn svg-elem
  "Replaces svg/svg, and removes warnings about xlink & react keys"
  [attribs & body]
  (into [:svg
         (svg/svg-attribs
          attribs
          {:xmlns "http://www.w3.org/2000/svg"})]
        body))

(defn svg [& args]
  (adapt/all-as-svg (apply svg-elem args)))
