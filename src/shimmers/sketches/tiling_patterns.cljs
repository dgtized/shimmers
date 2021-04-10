(ns shimmers.sketches.tiling-patterns
  (:require [shimmers.common.ui.controls :as ctrl]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.svg.adapter :as adapt]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.vector :as gv]))

(defn svg
  "Replaces svg/svg, and removes warnings about xlink & react keys"
  [attribs & body]
  (into [:svg
         (svg/svg-attribs
          attribs
          {:xmlns "http://www.w3.org/2000/svg"})]
        body))

;; palette from https://htmlcolors.com/palette/288/paleta-6
(def paleta-6 ["rgb(7,31,65)" "rgb(0,75,90)" "rgb(246,199,111)", "rgb(237,69,52)" "rgb(188,52,44)"])

(defn tiling-rect [n width palette]
  (let [rect (rect/rect 0 0 width width)]
    (for [i (range n)
          j (range n)]
      (-> rect
          (geom/translate (gv/vec2 (* i width) (* j width)))
          (with-meta {:fill (rand-nth palette)})))))

(defn scene []
  (let [a 0]
    (svg {:width 800 :height 600 :stroke "black"}
         (tiling-rect 5 20 paleta-6))))

(defn page []
  (adapt/all-as-svg (scene)))

(defn ^:export run-sketch []
  ;; 20210409
  (ctrl/mount page "svg-host"))
