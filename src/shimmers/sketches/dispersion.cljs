(ns shimmers.sketches.dispersion
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
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

(defn update-state [state]
  state)

(defn draw [_]
  (q/background 1.0)
  (q/stroke-weight 0.2)
  (let [building (rect/rect (cq/rel-w 0.1) (cq/rel-h 0.55) (cq/rel-w 0.2) (cq/rel-h 0.44))
        [_ ne _ sw] (geom/vertices building)
        max-dist (geom/dist ne sw)
        tessellated (geom/tessellate building {:num 32})
        divided (mapcat (fn [s]
                          (let [corner-dist (geom/dist (geom/centroid s) ne)]
                            (for [t (if (p/chance (* 0.01 (- (/ max-dist 1.9) corner-dist)))
                                      (geom/subdivide s)
                                      [s])]
                              (if (p/chance (* 0.03 (- (/ max-dist 1.5) corner-dist)))
                                (-> t
                                    (geometry/rotate-around-centroid (rand))
                                    (geom/translate (tm/* (gv/vec2 (* 0.9 (rand)) (* -0.6 (rand)))
                                                          (* (rand) (cq/rel-w 0.8)))))
                                t))))
                        tessellated)]
    (doseq [shape divided]
      (draw-polygon shape))))

(defn ^:export run-sketch []
  (q/defsketch dispersion
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
