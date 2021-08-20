(ns shimmers.sketches.slashes
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.algorithm.line-clipping :as clip]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn slash-region [bounds angle x0 n spacing]
  (let [{[bx by] :p [bw bh] :size} bounds
        m (Math/tan angle)
        cosa (Math/cos angle)
        c (- (+ by bh) (* m x0))
        x0 (- x0 (/ bw 2))
        y0 (+ (* m x0) c)
        x1 (+ bx bw (/ bw 2))
        y1 (+ (* m x1) c)]
    (loop [i 0 step 0 slashes []]
      (if (> i n)
        slashes
        (let [p (gv/vec2 x0 (- y0 step))
              q (gv/vec2 x1 (- y1 step))]
          (if-let [line (clip/clip-line bounds p q)]
            (recur (inc i) (+ step (/ (spacing) cosa)) (conj slashes line))
            slashes))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (rect/rect (cq/rel-pos 0 0) (cq/rel-pos 1 1))]
    {:slashes (slash-region bounds (tm/random 5.0 6.0) (cq/rel-w 0.8) 10 #(tm/random 5 15))}))

(defn update-state [state]
  state)

(defn draw [{:keys [slashes]}]
  (q/background 1.0)
  (doseq [{[p q] :points} slashes]
    (q/line p q)))

(sketch/defquil slashes
  :created-at "2021-08-20"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
