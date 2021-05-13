(ns shimmers.common.svg
  (:require [clojure.string :as str]
            [thi.ng.geom.svg.adapter :as adapt]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.math.core :as tm]))

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

(defn rotate [heading [x y]]
  (as-> [(tm/degrees heading) x y] o
    (str/join "," o)
    (str "rotate(" o ")")))
