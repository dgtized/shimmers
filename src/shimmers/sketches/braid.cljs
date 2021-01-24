(ns shimmers.sketches.braid
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.algorithm.helpers :refer [index-of]]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]))

(defn setup []
  {:rate 10.0
   :row 0})

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

(defn strand-changes [row]
  (let [strands (braid-row next-row row)
        next-strands (braid-row next-row (inc row))]
    (for [position [1 0 2]
          :let [value (nth strands position)]]
      [value position (index-of next-strands value)])))

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

(defn draw [{:keys [rate row]}]
  (let [rh 10 ;; row height
        cw 10 ;; column width
        percent (/ (mod (q/frame-count) (q/floor rate)) rate)]
    (when (= row 0)
      (q/background 255))
    (q/stroke-weight 3)
    (q/translate 290 (* row rh))

    (doseq [[value from to] (strand-changes row)]
      (color value)
      (cq/lerp-line [(* from cw) 0] [(* to cw) rh] percent))))

(defn ^:export run-sketch []
  (q/defsketch braid
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
