(ns shimmers.sketches.variations
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn hashmark [t]
  (map (fn [l] (-> l
                  (geom/translate (gv/vec2 -0.5 -0.5))
                  (geom/rotate t)))
       [(gl/line2 0.4 0.1 0.4 0.9)
        (gl/line2 0.6 0.1 0.6 0.9)
        (gl/line2 0.1 0.4 0.9 0.4)
        (gl/line2 0.1 0.6 0.9 0.6)]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {})

(defn update-state [state]
  state)

(defn draw-mark [pos scale]
  (doseq [{[p q] :points} (hashmark (/ (tm/mag-squared pos)(* (q/width) (q/height))))]
    (q/line (tm/+ pos (tm/* p scale))
            (tm/+ pos (tm/* q scale)))))

(defn draw [state]
  (q/background 1.0)
  (q/stroke-weight 1.0)
  (let [I 5
        J 6
        delta (tm/* (gv/vec2 (q/width) (q/height)) (gv/vec2 (/ 1 I) (/ 1 J)))]
    (doseq [i (range I)]
      (doseq [j (range J)]
        (draw-mark (tm/* (gv/vec2 (+ i 0.5) (+ j 0.5)) delta)
                   (/ (q/width) I))))))

(sketch/defquil variations
  :created-at "2021-08-25"
  :size [600 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
