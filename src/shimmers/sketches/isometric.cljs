(ns shimmers.sketches.isometric
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [thi.ng.geom.aabb :as aabb]
   [thi.ng.geom.core :as g]
   [shimmers.common.quil :as cq]))

(def iso-angle (/ (Math/sqrt 2) 2))

;; https://www.redblobgames.com/articles/coordinate-transforms/_2015/
(defn isometric [[x y]]
  (gv/vec2 (+ (* x iso-angle) (* y iso-angle))
           (* iso-angle (- (* y iso-angle) (* x iso-angle)))))

;; https://gamedev.stackexchange.com/questions/159434/how-to-convert-3d-coordinates-to-2d-isometric-coordinates
(def SQRT6 (Math/sqrt 6))
(defn isometric3 [[x y z]]
  (gv/vec2 (/ (- x z) tm/SQRT2)
           (/ (+ x (* 2 y) z) SQRT6)))

(defn isofaces [shape]
  (let [[a _ _ b c _] (g/faces shape)]
    [a b c]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0.0})

(defn update-state [state]
  (update state :t + 0.01))

(defn draw [{:keys [t]}]
  (q/background 1.0)
  (let [cube (g/translate (aabb/aabb 100) (gv/vec3 400 (* 100 (Math/sin t)) 100))]
    (doseq [face-pts (isofaces cube)]
      (cq/draw-shape (map isometric3 face-pts)))))

(sketch/defquil isometric-sketch
  :created-at "2021-10-21"
  :size [900 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
