(ns shimmers.sketches.six-of-one-half-dozen-of-the-other
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [thi.ng.math.core :as tm]
            [thi.ng.geom.vector :as gv]
            [thi.ng.geom.polygon :as gp]))

(defn polar [r theta]
  (geom/as-cartesian (gv/vec2 r theta)))

(defn hexagon [r]
  (gp/polygon2
   (for [theta (butlast (range 0 tm/TWO_PI (/ tm/TWO_PI 6)))]
     (polar r theta))))

(defn subdivide-hexagon3 [r]
  (let [r' (/ r 3)
        pos (- r r')
        hex (hexagon r')]
    (into [hex]
          (for [p (geom/vertices (hexagon pos))]
            (geom/translate hex p)))))

(defn subdivide-hexagon4 [r]
  (let [r' (/ r 4)
        pos (- r r')
        hex (hexagon r')]
    (concat [hex]
            (for [p (geom/vertices (hexagon pos))]
              (geom/translate hex p))
            (for [theta (range (/ tm/TWO_PI 12) tm/TWO_PI (/ tm/TWO_PI 6))]
              (geom/translate hex (polar (/ (* (Math/sqrt 3) r) 4) theta))))))

(defn subdivide-hexagon5 [r]
  (let [r' (/ r 5)
        hex (hexagon r')]
    (concat [hex]
            (for [theta (range 0 tm/TWO_PI (/ tm/TWO_PI 6))]
              (geom/translate hex (polar (/ (* 3 r) 5) theta)))
            (for [theta (range (/ tm/TWO_PI 12) tm/TWO_PI (/ tm/TWO_PI 6))]
              (geom/translate hex (polar (/ (* (Math/sqrt 3) r) 5) theta)))
            (for [theta (range (/ tm/TWO_PI 12) tm/TWO_PI (/ tm/TWO_PI 6))]
              (geom/translate hex (polar (/ (* 2 (Math/sqrt 3) r) 5) theta))))))

(defn subdivide-hexagon6 [r]
  (let [r' (/ r 6)
        hex (hexagon r')]
    (concat [hex]
            (for [theta (range 0 tm/TWO_PI (/ tm/TWO_PI 6))]
              (geom/translate hex (polar (/ r 2) theta)))
            (for [theta (range (/ tm/TWO_PI 12) tm/TWO_PI (/ tm/TWO_PI 6))]
              (geom/translate hex (polar (/ (* (Math/sqrt 3) r) 6) theta)))
            (for [theta (range (/ tm/TWO_PI 12) tm/TWO_PI (/ tm/TWO_PI 6))]
              (geom/translate hex (polar (/ (* 2 (Math/sqrt 3) r) 6) theta)))
            ;; wrong offsets to fill
            (for [theta (range 0 tm/TWO_PI (/ tm/TWO_PI 12))]
              (geom/translate hex (polar (/ (* 4 r) 5) (+ (/ tm/TWO_PI 6) theta)))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [base (* 0.5 (q/height))]
    {:shapes (concat [(hexagon base)]
                     (subdivide-hexagon6 base))}))

(defn update-state [state]
  state)

(defn draw [{:keys [shapes]}]
  (q/with-translation (cq/rel-pos 0.5 0.5)
    (doseq [shape shapes]
      (cq/draw-shape (geom/vertices shape)))))

(defn ^:export run-sketch []
  ;; 20210517
  (q/defsketch six-of-one-half-dozen-of-the-other
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
