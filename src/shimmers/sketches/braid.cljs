(ns shimmers.sketches.braid
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.algorithm.helpers :refer [index-of]]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]))

(defn setup []
  {:rate 10.0
   :row 0
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

(defn update-state [{:keys [rate row] :as state}]
  (if (= 0 (mod (q/frame-count) rate))
    (-> state
        (update :row + 1)
        (update :strands braid-row row)
        (update :row mod 40))
    state))

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
   position percent]
  (let [value (nth strands position)
        next-strands (braid-row strands row)
        x0 (x-offset center dw (index-of strands value))
        x1 (x-offset center dw (index-of next-strands value))]
    (color value)
    (cq/lerp-line [x0 (* row dh)]
                  [x1 (* (inc row) dh)]
                  percent)))

(defn draw [{:keys [rate row strands]}]
  (let [dh 10
        left 290
        cw 10
        percent (/ (mod (q/frame-count) (q/floor rate)) rate)]
    (when (= row 0)
      (q/background 255))
    (q/stroke-weight 3)

    (draw-strand strands left cw row dh 1 percent)
    (draw-strand strands left cw row dh 0 percent)
    (draw-strand strands left cw row dh 2 percent)
    ))

(defn ^:export run-sketch []
  (q/defsketch braid
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
