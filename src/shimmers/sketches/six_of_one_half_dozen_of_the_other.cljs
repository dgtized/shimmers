(ns shimmers.sketches.six-of-one-half-dozen-of-the-other
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.deterministic-random :as dr]
            [shimmers.math.hexagon :as hex :refer [hexagon]]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.polygon :as gp]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn polar [r theta]
  (geom/as-cartesian (gv/vec2 r theta)))

(defn hexagon->polygon [{:keys [p r]}]
  (-> (for [theta (butlast (range 0 tm/TWO_PI (/ tm/TWO_PI 6)))]
        (polar r theta))
      gp/polygon2
      (geom/translate p)))

(defn surrounding-hexes [hex radius]
  (for [theta (map (partial * tm/TWO_PI) (butlast (tm/norm-range 6)))]
    (geom/translate hex (polar radius theta))))

(defn subdivide-hexagon-inset
  "Returns a list of hexagons contained in or just touching the border of
  containing hexagon subdivided by `n`, where `n` >= 3."
  [{:keys [p r]} n]
  (let [r' (/ r n)
        hex (hexagon->polygon (hex/hexagon p (* r 0.999)))]
    (->> (hex/axial-range (- n 2))
         (map (comp (partial geom/translate (hex/hexagon p r'))
                    (partial hex/axial-flat->pixel r')))
         (filter (fn [s] (geom/contains-point? hex (:p s)))))))

(defn subdivide-hexagon3-outside [{:keys [p r]}]
  (let [r' (/ r 3)
        hex (hexagon p r')]
    (into [hex]
          (surrounding-hexes hex (* 3 r')))))

(defn maybe-subdivide [shape]
  (let [subdiv (dr/weighted {(fn [s] (subdivide-hexagon-inset s 3)) 32
                             subdivide-hexagon3-outside 1
                             (fn [s] (subdivide-hexagon-inset s 4)) 16
                             (fn [s] (subdivide-hexagon-inset s 5)) 8
                             (fn [s] (subdivide-hexagon-inset s 6)) 4})]
    (if-not (:divided shape)
      (into [(assoc shape :divided true)]
            (dr/map-random-sample (constantly (/ 1 36))
                                  (fn [_] nil)
                                  (subdiv shape)))
      [shape])))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [p (gv/vec2)
        ;; height is 1/sqrt(3) to fit exactly, so scale it down by a hair
        r (* (/ 0.99 (Math/sqrt 3)) (q/height))
        start (hexagon p r)]
    ;; Chance of *two* root hexagons, so patterns can fill in from underneath
    {:shapes (into [start]
                   (dr/weighted {[] 5
                                 (subdivide-hexagon-inset start (dr/drand-int 3 7)) 4}))}))

(defn update-state [state]
  (if (< (count (:shapes state)) 1200)
    (update state :shapes (partial dr/mapcat-random-sample
                                   (fn [s] (/ (:r s) (q/height)))
                                   maybe-subdivide))
    state))

(defn draw [{:keys [shapes]}]
  (q/stroke-weight 0.5)
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
