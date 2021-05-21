(ns shimmers.sketches.six-of-one-half-dozen-of-the-other
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.probability :as p]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.polygon :as gp]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn polar [r theta]
  (geom/as-cartesian (gv/vec2 r theta)))

(defn hex-range [n t0]
  (let [r (range (* tm/TWO_PI t0) tm/TWO_PI (/ tm/TWO_PI n))]
    (if (tm/delta= (last r) tm/TWO_PI)
      (butlast r)
      r)))

(defn hexagon->polygon [{:keys [p r]}]
  (-> (for [theta (butlast (range 0 tm/TWO_PI (/ tm/TWO_PI 6)))]
        (polar r theta))
      gp/polygon2
      (geom/translate p)))

(defn hexagon [p r]
  (gc/circle p r))

(defn surrounding-hexes [hex phase radius]
  (for [theta (hex-range 6 phase)]
    (geom/translate hex (polar radius theta))))

;; https://www.redblobgames.com/grids/hexagons/
(defn coord->hex [size [q r]]
  (tm/* (gv/vec2 (* q (/ 3 2))
                 (+ (* q 0.5 (Math/sqrt 3)) (* r (Math/sqrt 3))))
        size))

(defn subdivide-hexagon3 [p r]
  (let [r' (/ r 3)
        hex (hexagon p r')]
    (into [hex]
          (surrounding-hexes hex 0 (* 2 r')))))

(defn subdivide-hexagon3-outside [p r]
  (let [r' (/ r 3)
        hex (hexagon p r')]
    (into [hex]
          (surrounding-hexes hex 0 (* 3 r')))))

(defn subdivide-hexagon4 [p r]
  (let [r' (/ r 4)
        hex (hexagon p r')]
    (concat [hex]
            (surrounding-hexes hex 0 (* 3 r'))
            (surrounding-hexes hex (/ 1 12) (* (Math/sqrt 3) r')))))

(defn subdivide-hexagon5 [p r]
  (let [r' (/ r 5)
        hex (hexagon p r')]
    (concat [hex]
            (surrounding-hexes hex 0 (* 3 r'))
            (surrounding-hexes hex (/ 1 12) (* (Math/sqrt 3) r'))
            (surrounding-hexes hex (/ 1 12) (* 2 (Math/sqrt 3) r')))))

(defn subdivide-hexagon6 [p r]
  (let [r' (/ r 6)
        hex (hexagon p r')]
    (concat [hex]
            (surrounding-hexes hex 0 (* 3 r'))
            (surrounding-hexes hex (/ 1 12) (* (Math/sqrt 3) r'))
            (surrounding-hexes hex (/ 1 12) (* 2 (Math/sqrt 3) r'))
            (for [coord [[2 1] [1 2] [-1 3] [-2 3] [-3 2] [-3 1]
                         [-2 -1] [-1 -2] [1 -3] [2 -3] [3 -2] [3 -1]]]
              (geom/translate hex (coord->hex r' coord))))))

(defn maybe-subdivide [shape]
  (let [subdiv (p/weighted {subdivide-hexagon3 32
                            subdivide-hexagon3-outside 1
                            subdivide-hexagon4 16
                            subdivide-hexagon5 16
                            subdivide-hexagon6 8})]
    (if-not (:divided shape)
      (into [(assoc shape :divided true)] (subdiv (:p shape) (:r shape)))
      [shape])))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [p (gv/vec2)
        ;; height is 1/sqrt(3) to fit exactly, so scale it down by a hair
        r (* (/ 0.99 (Math/sqrt 3)) (q/height))]
    ;; Chance of *two* root hexagons, so patterns can fill in from underneath
    {:shapes (into [(hexagon p r)]
                   (p/weighted {[] 3
                                (subdivide-hexagon3 p r) 1
                                (subdivide-hexagon4 p r) 1
                                (subdivide-hexagon5 p r) 1}))}))

(defn update-state [state]
  (if (< (count (:shapes state)) 1200)
    (update state :shapes (partial p/mapcat-random-sample
                                   (fn [s] (/ (:r s) (q/height)))
                                   maybe-subdivide))
    state))

(defn draw [{:keys [shapes]}]
  (q/stroke-weight 0.66)
  (q/with-translation (cq/rel-pos 0.5 0.5)
    (doseq [shape shapes]
      (->> shape
           hexagon->polygon
           geom/vertices
           cq/draw-shape))))

(defn ^:export run-sketch []
  ;; 20210517
  (q/defsketch six-of-one-half-dozen-of-the-other
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
