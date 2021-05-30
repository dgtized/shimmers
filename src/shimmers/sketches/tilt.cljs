(ns shimmers.sketches.tilt
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.geometry :as geometry]
            [shimmers.math.hexagon :as hex]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
            [thi.ng.geom.polygon :as gp]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn stalactite []
  (let [x1 (- (* 1.1 (rand)) 0.1)
        x2 (+ x1 (* 0.09 (q/random-gaussian)))
        x3 (+ x1 (* 0.3 (rand)))
        y3 (+ (* 0.4 x3) 0.2 (* 0.03 (q/random-gaussian)))]
    (gt/triangle2 (cq/rel-pos x1 1.0)
                  (cq/rel-pos x2 1.0)
                  (cq/rel-pos x3 y3))))

(defn bokeh [d]
  (let [p (cq/rel-pos 0.0 0.15)
        q (cq/rel-pos 1.0 0.28)]
    (-> (geom/point-at (gl/line2 p q) d)
        (geom/translate (gv/vec2 (* 2 (q/random-gaussian))
                                 (* 8 (q/random-gaussian))))
        (hex/hexagon (+ 20 (q/random-gaussian)))
        (geom/vertices 6)
        gp/polygon2
        (geometry/rotate-around-centroid (rand)))))

(defn pow-range [n p]
  (map #(Math/pow % p) (tm/norm-range n)))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:shapes (concat (repeatedly 96 stalactite)
                   (map (comp bokeh #(- 1.0 %))
                        (pow-range 24 1.5)))})

(defn update-state [state]
  state)

;; Consider adding spheres of confusion somehow, ie circles or hexagon bokeh?
(defn draw [{:keys [shapes]}]
  (q/background 1.0)
  (q/no-stroke)
  (q/fill 0.0 0.3)
  (doseq [shape shapes]
    (cq/draw-shape (geom/vertices shape))))

(defn ^:export run-sketch []
  ;; 20210530
  (q/defsketch tilt
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
