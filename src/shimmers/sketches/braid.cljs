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

(defn next-row [[[a b c] row]]
  (if (= 0 (mod row 2))
    [[a c b] (inc row)]
    [[b a c] (inc row)]))

(defn braid-row [braid-fn row]
  (first (nth (iterate braid-fn [[0 1 2] 0]) row)))

(defn update-state [{:keys [rate] :as state}]
  (if (= 0 (mod (q/frame-count) rate))
    (-> state
        (update :row + 1)
        (update :row mod 40))
    state))

(defn color [value]
  (let [low 64 high 192]
    (condp = value
      0 (q/stroke high low low)
      1 (q/stroke low high low)
      2 (q/stroke low low high))))

(defn draw-strand
  [cw rh row position percent]
  (let [strands (braid-row next-row row)
        next-strands (braid-row next-row (inc row))
        value (nth strands position)
        x0 (* cw (index-of strands value))
        x1 (* cw (index-of next-strands value))]
    (color value)
    (cq/lerp-line [x0 0] [x1 rh] percent)))

(defn draw [{:keys [rate row]}]
  (let [rh 10 ;; row height
        cw 10 ;; column width
        percent (/ (mod (q/frame-count) (q/floor rate)) rate)]
    (when (= row 0)
      (q/background 255))
    (q/stroke-weight 3)
    (q/translate 290 (* row rh))

    (draw-strand cw rh row 1 percent)
    (draw-strand cw rh row 0 percent)
    (draw-strand cw rh row 2 percent)
    ))

(defn ^:export run-sketch []
  (q/defsketch braid
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
