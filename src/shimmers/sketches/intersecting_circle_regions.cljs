(ns shimmers.sketches.intersecting-circle-regions
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.utils.intersect :as isec]
   [thi.ng.math.core :as tm]))

(defn arc [circle t0 t1 res]
  (for [t (if (pos? res)
            (range t0 (if (< t0 t1) t1 (+ t1 eq/TAU)) res)
            (range t0 (if (> t0 t1) t1 (- t1 eq/TAU)) res))]
    (g/point-at circle (/ t eq/TAU))))

(defn half-moon [{pa :p :as a} {pb :p :as b} up down res]
  (gp/polygon2
   (concat (arc a
                (g/heading (tm/- up pa))
                (g/heading (tm/- down pa))
                res)
           (arc b
                (g/heading (tm/- down pb))
                (g/heading (tm/- up pb))
                (- res)))))

(defn intersect-moon [{p :p :as a} {q :p :as b} up down res]
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
         (intersect-moon a b up down res)
         (half-moon b a down up res)])
      (if (< ra rb)
        [(qdg/contour-polygon b [a]) a]
        [(qdg/contour-polygon a [b]) b]))
    [a b]))

(defn make-circles [t]
  [(gc/circle (cq/rel-vec (+ 0.4 (* 0.1 (Math/sin t))) 0.5) (cq/rel-h 0.4))
   (gc/circle (cq/rel-vec (- 0.6 (* 0.3 (Math/sin t))) 0.5) (cq/rel-h 0.3))])

(defn setup []
  (q/color-mode :hsl 1.0)
  {:circles (make-circles 0.0)})

(defn update-state [{:keys [t] :as state}]
  (-> state
      (update :t + 0.01)
      (assoc :circles (make-circles t))))

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

;; TODO: https://hogg.io/writings/circle-intersections for constructing regions
;; from N intersecting circles by converting to graphs. Also, need to add a
;; CompositePath / CompositePolygon type to allow line segments or arc segments.
(defn ui-controls []
  [:div
   [:p "Genuary 2023 Day 5 - Debug View"]
   [:p "Shows the disjoint polygon regions constructed from two circles
   intersecting eachother."]])

(sketch/defquil intersecting-circle-regions
  :created-at "2023-01-05"
  :tags #{:genuary2023}
  :on-mount (fn [] (ctrl/mount ui-controls))
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
