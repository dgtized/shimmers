(ns shimmers.sketches.dispersion
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/no-loop)
  (q/color-mode :hsl 1.0)
  {})

(defn rdirection []
  (tm/* (gv/vec2 (* 0.6 (abs (dr/gaussian 0 3.5)))
                 (* -0.4 (abs (dr/gaussian 0 3.5))))
        (cq/rel-w 0.09)))

(defn epicenter-distance [epicenter max-dist s]
  (/ (g/dist epicenter (g/centroid s)) max-dist))

(defn possibly-subdivide [p-fn shapes]
  (mapcat (fn [s]
            (if (dr/chance (p-fn s))
              (g/subdivide s)
              [s]))
          shapes))

(defn possibly-disperse [p-fn displacement shapes]
  (map (fn [s]
         (if (dr/chance (p-fn s))
           (geometry/displace s (dr/random) (displacement s))
           s))
       shapes))

(defn draw [_]
  (q/background 1.0)
  (q/stroke-weight 0.2)
  (let [building (rect/rect (cq/rel-pos 0.1 0.4) (cq/rel-w 0.3) (cq/rel-h 0.6))
        [_ ne _ sw] (g/vertices building)
        max-dist (g/dist ne sw)
        tessellated (g/tessellate building {:num 48})
        divided (possibly-subdivide (fn [s] (- 0.8 (epicenter-distance ne max-dist s))) tessellated)
        shapes (possibly-disperse (fn [s] (- 2.0 (epicenter-distance ne (/ max-dist 2.8) s)))
                                  (fn [_] (rdirection))
                                  divided)]
    (doseq [shape shapes]
      (q/stroke-weight 0.02)
      (q/fill (+ (* 0.005 (dr/gaussian 0 3.5)) 0.55)
              (+ (* 0.01 (dr/gaussian 0 3.5)) 0.6)
              (-> (epicenter-distance sw (q/width) shape)
                  (tm/clamp 0.25 0.75))
              0.9)
      (cq/draw-polygon shape))))

(defn page []
  [:p.center (view-sketch/generate :dispersion)])

(sketch/defquil dispersion
  :created-at "2021-03-10"
  :tags #{:static :deterministic}
  :on-mount #(ctrl/mount page)
  :size [900 600]
  :setup setup
  :draw draw
  :middleware [m/fun-mode])
