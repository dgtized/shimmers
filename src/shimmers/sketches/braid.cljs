(ns shimmers.sketches.braid
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.sequence :refer [index-of]]
   [shimmers.sketch :as sketch :include-macros true]))

(defn color [value]
  (let [low 64 high 192]
    (condp = value
      0 [high low low]
      1 [low high low]
      2 [low low high])))

;;   0 1 2
;; L 1 0 2
;; R 1 2 0
;; L 2 1 0
;; R 2 0 1
;; L 0 2 1
;; R 0 1 2
(defn classic-3-strand [row]
  (let [next-row
        (fn [[[a b c] row]]
          (if (= 0 (mod row 2))
            [[a c b] (inc row)]
            [[b a c] (inc row)]))
        braid-row
        (fn [row] (first (nth (iterate next-row [[0 1 2] 0]) row)))
        strands (braid-row row)
        next-strands (braid-row (inc row))]
    (for [position [1 0 2]
          :let [value (nth strands position)]]
      [(color value) position (index-of next-strands value)])))

(defn candy-cane [row]
  [[(color (mod row 2)) 0 1]
   [(color (mod (inc row) 2)) 1 0]])

(defn setup []
  {:braids [classic-3-strand candy-cane classic-3-strand]})

(defn draw [{:keys [braids]}]
  (let [rate 20
        rh 10 ;; row height
        cw 10 ;; column width
        fc (q/frame-count)
        row (mod (q/floor (/ fc rate)) (/ (q/height) rh))
        percent (/ (mod fc rate) rate)
        even-split (/ (q/width) (count braids))]
    (when (= row 0)
      (q/background 255))
    (q/stroke-weight 3)

    (q/translate (- (/ even-split 2)) 0)
    (doseq [strand-changes braids]
      (q/translate even-split 0)
      (q/with-translation [0 (* row rh)]
        (doseq [[color from to] (strand-changes row)]
          (apply q/stroke color)
          (cq/lerp-line [(* from cw) 0] [(* to cw) rh] percent))))))

(sketch/defquil braid
  :created-at "2021-01-23"
  :size [600 400]
  :setup setup
  :draw draw
  :middleware [m/fun-mode framerate/mode])
