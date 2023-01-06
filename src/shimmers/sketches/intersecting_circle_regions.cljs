(ns shimmers.sketches.intersecting-circle-regions
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.utils.intersect :as isec]
   [thi.ng.math.core :as tm]))

(defn arc [circle t0 t1 res]
  (for [t (range t0 (if (< t0 t1) t1 (+ t1 eq/TAU)) res)]
    (g/point-at circle (/ t eq/TAU))))

(defn half-moon [{p :p :as a} {q :p :as b} up down res]
  (gp/polygon2
   (concat (arc a
                (g/heading (tm/- up p))
                (g/heading (tm/- down p))
                res)
           [down]
           (reverse (arc b
                         (g/heading (tm/- up q))
                         (g/heading (tm/- down q))
                         res)))))

(defn intersection [{p :p :as a} {q :p :as b} up down res]
  (gp/polygon2
   (concat
    (arc a
         (g/heading (tm/- down p))
         (g/heading (tm/- up p))
         res)
    (arc b
         (g/heading (tm/- up q))
         (g/heading (tm/- down q))
         res))))

(defn regions [{ra :r :as a} {rb :r :as b} res]
  (if (collide/overlaps? a b)
    (if-let [contacts (isec/intersect-circle-circle? a b)]
      (let [[up down] contacts]
        [(half-moon a b up down res)
         (intersection a b up down res)
         (half-moon b a down up res)])
      (if (< ra rb)
        [a]
        [b]))
    [a b]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:circles [(gc/circle (cq/rel-vec 0.35 0.5) (cq/rel-h 0.4))
             (gc/circle (cq/rel-vec 0.65 0.5) (cq/rel-h 0.3))]})

(defn update-state [state]
  state)

(defn draw [{:keys [circles]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  (q/stroke 0.0)
  (q/stroke-weight 1.0)
  (doseq [circle circles]
    (cq/circle circle))

  (q/stroke 0.5)
  (doseq [[i region] (map-indexed vector (regions (first circles) (second circles) (/ eq/TAU 12)))]
    (q/fill (mod (* tm/PHI i) 1.0) 0.5 0.5 0.1)
    (qdg/draw region)))

(sketch/defquil intersecting-circle-regions
  :created-at "2023-01-05"
  :tags #{:genuary2023}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
