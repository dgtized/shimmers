(ns shimmers.sketches.slashes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.algorithm.line-clipping :as clip]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

;; TODO: change x0 a percent of width + height of shape to find start of slashes
(defn slash-region [bounds angle x0 n spacing width]
  (let [{[bx by] :p [bw bh] :size} bounds
        m (Math/tan angle)
        cosa (Math/cos angle)
        c (- (+ by bh) (* m x0))
        x0 (- bx (/ bw 2))
        y0 (+ (* m x0) c)
        x1 (+ bx bw (/ bw 2))
        y1 (+ (* m x1) c)]
    (loop [i 0 step 0 slashes []]
      (if (< i n)
        (let [p (gv/vec2 x0 (- y0 step))
              q (gv/vec2 x1 (- y1 step))
              s (spacing)
              w (width)]
          (if-let [line (clip/clip-line bounds p q)]
            (recur (inc i) (+ step (/ (+ s (/ w 2)) cosa))
                   (conj slashes (assoc line :width w)))
            slashes))
        slashes))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (rect/rect (cq/rel-pos 0 0) (cq/rel-pos 1 1))]
    {:slashes
     (concat (slash-region bounds (tm/random 5.0 6.0) 0
                           (int (tm/random 6 16)) (constantly 10) (constantly 1))
             (slash-region bounds (tm/random 5.0 6.0) (cq/rel-w 0.8)
                           10 #(tm/random 5 15) #(tm/random 0.8 6))
             (slash-region bounds (tm/random 3.5 4.5) (cq/rel-w 0.4)
                           (int (tm/random 4 16)) #(tm/random 5 15) #(tm/random 0.5 4))
             (slash-region bounds (tm/random 3.5 4.5) (* (g/width bounds) 1.3)
                           8 #(tm/random 5 15) #(tm/random 0.5 2)))}))

(defn update-state [state]
  state)

(defn draw [{:keys [slashes]}]
  (q/background 1.0)
  (doseq [{[p q] :points width :width} slashes]
    (q/stroke-weight width)
    (q/line p q)))

(sketch/defquil slashes
  :created-at "2021-08-20"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
