(ns shimmers.sketches.deformed-spirals
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.math.core :as tm]))

;; Concept was to make nearby spirals deform eachother like a gravity well, but
;; just doing a noise deformation was interesting on it's own.

(defn spiral [center dr dtheta steps t]
  (for [theta (range 0 (* steps dtheta) dtheta)]
    (let [pos (v/polar (* dr (/ theta tm/TWO_PI)) (+ theta (* 1.5 t)))
          [nx ny] (tm/div pos 192)
          n (q/noise nx ny t)]
      (tm/+ center (tm/+ pos (v/polar 40 (* n tm/TWO_PI)))))))

(defn draw [_]
  (q/color-mode :hsl 1.0)
  (q/background 1.0)
  (q/stroke-weight 0.8)
  (q/no-fill)
  (-> (cq/rel-vec 0.5 0.5)
      (spiral 9.0 0.9 425 (/ (q/frame-count) 800))
      cq/draw-curve-path))

(defn page []
  (sketch/component
   :size [800 600]
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition deformed-spirals
  {:created-at "2021-10-10"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
