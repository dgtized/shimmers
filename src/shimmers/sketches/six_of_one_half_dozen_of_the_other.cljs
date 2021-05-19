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

(defn subdivide-hexagon3 [p r]
  (let [r' (/ r 3)
        pos (- r r')
        hex (hexagon p r')]
    (into [hex]
          (for [theta (hex-range 6 0)]
            (geom/translate hex (polar pos theta))))))

(defn subdivide-hexagon4 [p r]
  (let [r' (/ r 4)
        pos (- r r')
        hex (hexagon p r')]
    (concat [hex]
            (for [theta (hex-range 6 0)]
              (geom/translate hex (polar pos theta)))
            (for [theta (hex-range 6 (/ 1 12))]
              (geom/translate hex (polar (/ (* (Math/sqrt 3) r) 4) theta))))))

(defn subdivide-hexagon5 [p r]
  (let [r' (/ r 5)
        hex (hexagon p r')]
    (concat [hex]
            (for [theta (hex-range 6 0)]
              (geom/translate hex (polar (/ (* 3 r) 5) theta)))
            (for [theta (hex-range 6 (/ 1 12))]
              (geom/translate hex (polar (/ (* (Math/sqrt 3) r) 5) theta)))
            (for [theta (hex-range 6 (/ 1 12))]
              (geom/translate hex (polar (/ (* 2 (Math/sqrt 3) r) 5) theta))))))

;; FIXME: incorrect outer edges
(defn subdivide-hexagon6 [p r]
  (let [r' (/ r 6)
        hex (hexagon p r')]
    (concat [hex]
            (for [theta (hex-range 6 0)]
              (geom/translate hex (polar (/ r 2) theta)))
            (for [theta (hex-range 6 (/ 1 12))]
              (geom/translate hex (polar (/ (* (Math/sqrt 3) r) 6) theta)))
            (for [theta (hex-range 6 (/ 1 12))]
              (geom/translate hex (polar (/ (* 2 (Math/sqrt 3) r) 6) theta)))
            ;; wrong offsets to fill
            (for [theta (hex-range 6 (/ 5 36))]
              (geom/translate hex (polar (/ (* 28 r) 36) theta)))
            (for [theta (hex-range 6 (/ 7 36))]
              (geom/translate hex (polar (/ (* 28 r) 36) theta))))))

(defn maybe-subdivide [shape]
  (let [subdiv (p/weighted {subdivide-hexagon3 2
                            subdivide-hexagon4 1
                            subdivide-hexagon5 1})]
    (if-not (:divided shape)
      (into [(assoc shape :divided true)] (subdiv (:p shape) (:r shape)))
      [shape])))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [p (gv/vec2)
        r (* 0.5 (q/height))]
    {:shapes (concat [(hexagon p r)]
                     (subdivide-hexagon5 p r))}))

(defn update-state [state]
  (if (< (count (:shapes state)) 1200)
    (update state :shapes (partial p/mapcat-random-sample
                                   (fn [s] (/ (:r s) (q/height)))
                                   maybe-subdivide))
    state))

(defn draw [{:keys [shapes]}]
  (q/with-translation (cq/rel-pos 0.5 0.5)
    (doseq [shape shapes]
      (cq/draw-shape (geom/vertices (hexagon->polygon shape))))))

(defn ^:export run-sketch []
  ;; 20210517
  (q/defsketch six-of-one-half-dozen-of-the-other
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
