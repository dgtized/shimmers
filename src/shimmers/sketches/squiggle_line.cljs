(ns shimmers.sketches.squiggle-line
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.line :as gl]))

;; Implementing squiggly line drawing as described in
;; https://rmarcus.info/blog/2017/10/23/humanlines.html
;; https://rmarcus.info/blog/assets/humanLines/Meraj08.pdf
;; https://github.com/RyanMarcus/humanLines/blob/master/index.js

(defn rel-line [{[p q] :points}]
  (gl/line2 (cq/rel-vec p) (cq/rel-vec q)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:lines (mapv rel-line [(gl/line2 0.2 0.2 0.8 0.2)
                          (gl/line2 0.3 0.3 0.6 0.3)
                          (gl/line2 0.7 0.25 0.7 0.6)
                          (gl/line2 0.8 0.35 0.8 0.8)
                          (gl/line2 0.2 0.7 0.5 0.4)
                          (gl/line2 0.5 0.5 0.6 0.7)])})

(defn update-state [state]
  state)

(defn draw [{:keys [lines]}]
  (doseq [{[p q] :points} lines]
    (q/line p q)))

(sketch/defquil squiggle-line
  :created-at "2021-"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
