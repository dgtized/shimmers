(ns shimmers.sketches.braid
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.algorithm.helpers :refer [index-of]]))

(defn setup []
  (q/frame-rate 10)
  {:row 0
   :strands [0 1 2]})

;;   0 1 2
;; L 1 0 2
;; R 1 2 0
;; L 2 1 0
;; R 2 0 1
;; L 0 2 1
;; R 0 1 2

(defn braid-row [[a b c] row]
  (if (= 0 (mod row 2))
    [a c b]
    [b a c]))

(defn update-state [{:keys [row] :as state}]
  (-> state
      (update :row + 1)
      (update :strands braid-row row)
      (update :row mod 40)))

(defn color [value]
  (let [low 64 high 192]
    (condp = value
      0 (q/stroke high low low)
      1 (q/stroke low high low)
      2 (q/stroke low low high))))

(defn x-offset [left cw index]
  (+ left (* cw index)))

(defn draw-strand
  [strands
   center dw row dh
   position]
  (let [value (nth strands position)
        next-strands (braid-row strands row)
        x0 (x-offset center dw (index-of strands value))
        x1 (x-offset center dw (index-of next-strands value))]
    (color value)
    (q/line x0 (* row dh)
            x1 (* (inc row) dh))))

(defn draw [{:keys [row strands]}]
  (let [dh 10
        left 290
        cw 10]
    (when (= row 0)
      (q/background 255))
    (q/stroke-weight 3)

    (draw-strand strands left cw row dh 1)
    (draw-strand strands left cw row dh 0)
    (draw-strand strands left cw row dh 2)
    ))

(defn ^:export run-sketch []
  (q/defsketch braid
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
