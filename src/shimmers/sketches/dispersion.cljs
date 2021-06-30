(ns shimmers.sketches.dispersion
  (:require [kixi.stats.distribution :as ksd]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.quil :as cq]
            [shimmers.math.geometry :as geometry]
            [shimmers.math.probability :as p]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn draw-polygon [poly]
  (cq/draw-shape (geom/vertices poly)))

(defn setup []
  (q/no-loop)
  (q/color-mode :hsl 1.0)
  {})

(defn rdirection [distrib]
  (tm/* (gv/vec2 (* 0.6 (Math/abs (ksd/draw distrib)))
                 (* -0.4 (Math/abs (ksd/draw distrib))))
        (cq/rel-w 0.09)))

(defn epicenter-distance [epicenter max-dist s]
  (/ (geom/dist epicenter (geom/centroid s)) max-dist))

(defn possibly-subdivide [p-fn shapes]
  (mapcat (fn [s]
            (if (p/chance (p-fn s))
              (geom/subdivide s)
              [s]))
          shapes))

(defn possibly-disperse [p-fn displacement shapes]
  (map (fn [s]
         (if (p/chance (p-fn s))
           (geometry/displace s (rand) (displacement s))
           s))
       shapes))

(defn draw [_]
  (q/background 1.0)
  (q/stroke-weight 0.2)
  (let [building (rect/rect (cq/rel-pos 0.1 0.4) (cq/rel-w 0.3) (cq/rel-h 0.6))
        [_ ne _ sw] (geom/vertices building)
        max-dist (geom/dist ne sw)
        tessellated (geom/tessellate building {:num 48})
        distribution (ksd/normal {:sd 3.5})
        divided (possibly-subdivide (fn [s] (- 0.8 (epicenter-distance ne max-dist s))) tessellated)
        shapes (possibly-disperse (fn [s] (- 2.0 (epicenter-distance ne (/ max-dist 2.8) s)))
                                  (fn [_] (rdirection distribution))
                                  divided)]
    (doseq [shape shapes]
      (q/stroke-weight 0.02)
      (q/fill (+ (* 0.005 (ksd/draw distribution)) 0.55)
              (+ (* 0.01 (ksd/draw distribution)) 0.6)
              (-> (epicenter-distance sw (q/width) shape)
                  (tm/clamp 0.25 0.75))
              0.9)
      (draw-polygon shape))))

(sketch/defquil dispersion
  :created-at "2021-03-10"
  :tags #{:static}
  :size [900 600]
  :setup setup
  :draw draw
  :middleware [m/fun-mode])
