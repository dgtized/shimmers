(ns shimmers.sketches.tiling-patterns
  (:require [shimmers.common.ui.controls :as ctrl]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.svg.adapter :as adapt]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

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

(defn seed-rect [rows cols palette]
  (for [i (range rows)
        j (range cols)]
    {:pos (gv/vec2 i j) :fill (rand-nth palette)}))

(defn cells->svg-rect [cells size]
  (let [rect (rect/rect 0 0 size size)]
    (for [{:keys [pos fill]} cells
          :let [[i j] pos]]
      (-> rect
          (geom/translate (tm/* pos (gv/vec2 size size)))
          (with-meta {:fill fill :key (str "cell-" i "-" j)})))))

(defn scene []
  (let [a 0]
    (svg {:width 800 :height 600 :stroke "black"}
         (cells->svg-rect (seed-rect 5 5 paleta-6) 20))))

(defn page []
  (adapt/all-as-svg (scene)))

(defn ^:export run-sketch []
  ;; 20210409
  (ctrl/mount page "svg-host"))
