(ns shimmers.sketches.dispersion
  (:require [kixi.stats.distribution :as ksd]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.quil :as cq]
            [shimmers.math.geometry :as geometry]
            [shimmers.math.probability :as p]
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

(defn draw [_]
  (q/background 1.0)
  (q/stroke-weight 0.2)
  (let [building (rect/rect (cq/rel-w 0.1) (cq/rel-h 0.5) (cq/rel-w 0.3) (cq/rel-h 0.5))
        [_ ne _ sw] (geom/vertices building)
        max-dist (geom/dist ne sw)
        tessellated (geom/tessellate building {:num 48})
        distribution (ksd/normal {:sd 3.5})
        divided (mapcat (fn [s]
                          (let [corner-dist (geom/dist (geom/centroid s) ne)]
                            (for [t (if (p/chance (* 0.01 (- (/ max-dist 1.9) corner-dist)))
                                      (geom/subdivide s)
                                      [s])]
                              (if (p/chance (* 0.03 (- (/ max-dist 1.5) corner-dist)))
                                (geometry/displace t (rand) (rdirection distribution))
                                t))))
                        tessellated)]
    (doseq [shape divided]
      (q/stroke-weight 0.02)
      (q/fill (+ (* 0.005 (ksd/draw distribution)) 0.55)
              (+ (* 0.01 (ksd/draw distribution)) 0.6)
              (-> (geom/centroid shape)
                  (geom/dist (gv/vec2 0 (q/height)))
                  (/ (q/width))
                  (tm/clamp 0.2 0.75))
              0.9)
      (draw-polygon shape))))

(defn ^:export run-sketch []
  ;; 20210310
  (q/defsketch dispersion
    :host "quil-host"
    :size [600 400]
    :setup setup
    :draw draw
    :middleware [m/fun-mode]))
