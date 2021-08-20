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
  (loop [x x0 n n slashes []]
    (if (zero? n)
      slashes
      (let [{[bx by] :p [bw bh] :size} bounds
            y0 (+ by bh)
            x1 (+ bx bw)
            m (Math/sin angle)
            p (gv/vec2 x y0)
            q (gv/vec2 x1 (+ (* m x1) y0))]
        (if-let [line (clip/clip-line bounds p q)]
          (recur (+ x spacing) (dec n) (conj slashes line))
          slashes)))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (rect/rect (cq/rel-pos 0 0) (cq/rel-pos 1 1))]
    {:slashes (slash-region bounds (tm/random 4.9 5.5) (cq/rel-w 0.6) 10 8)}))

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
