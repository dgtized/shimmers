(ns shimmers.sketches.isometric
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.aabb :as aabb]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; See also orthographic perspective:
;; https://www.scratchapixel.com/lessons/3d-basic-rendering/perspective-and-orthographic-projection-matrix/building-basic-perspective-projection-matrix

(def iso-angle (/ (Math/sqrt 2) 2))

;; https://www.redblobgames.com/articles/coordinate-transforms/_2015/
(defn isometric2 [[x y]]
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
  {:t 0.0
   :grid (g/subdivide (cq/screen-rect iso-angle) {:rows 16 :cols 16})})

(defn update-state [state]
  (update state :t + 0.002))

;; video projection of greyscale?
(defn draw [{:keys [grid t]}]
  (q/background 1.0)
  (let [m 0.002
        shapes (for [{[x y] :p [w h] :size} grid
                     :let [n (q/noise (* x m) (* y m) t)]]
                 (g/translate (aabb/aabb w)
                              (gv/vec3 (+ x (cq/rel-w 0.35))
                                       (- (* (/ h iso-angle) (Math/sin (* tm/TWO_PI n))) (cq/rel-h 0.17))
                                       (- (/ y iso-angle) (cq/rel-h 0.5)))))]
    (doseq [shape shapes]
      (doseq [face-pts (isofaces shape)]
        (cq/draw-shape (map isometric3 face-pts))))))

(sketch/defquil isometric
  :created-at "2021-10-21"
  :size [900 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
