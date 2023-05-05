(ns shimmers.sketches.stem-and-leaf
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; The intended concept is to plot out bezier curves from a source circle or
;; stem, and then add circles that are tangent to the curve, and then randomly
;; extend new bezier curves out from the circles generated before. Presumably
;; those curves should not intersect, and neither should the circles so there is
;; some circle packing as well. It's likely there is going to be a lot of S
;; curves with tangents on one side mirrored on the "outside". Also, each circle
;; may need to have a "direction" of rotation that curves emit from so they pair
;; up.

;; Right now it's just playing with straight lines from tangent points, but the
;; circles are arranged somewhat analagous to perspective lines to focal points,
;; so alternatively could make a different sketch around rotating perspectives.

(defonce defo (debug/state))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:circles [(gc/circle (cq/rel-pos 0.5 0.5) (cq/rel-h 0.1))
             (assoc (gc/circle (cq/rel-pos 0.2 0.5) (cq/rel-h 0.15)) :parent 0)
             (assoc (gc/circle (cq/rel-pos 0.8 0.5) (cq/rel-h 0.05)) :parent 0)
             (assoc (gc/circle (cq/rel-pos 0.5 0.2) (cq/rel-h 0.15)) :parent 0)
             (assoc (gc/circle (cq/rel-pos 0.5 0.8) (cq/rel-h 0.05)) :parent 0)]})

(defn update-state [state]
  state)

(defn tangent-lines [c1 c2]
  (let [{:keys [p r]} c1
        {p' :p r' :r} c2
        angle (+ tm/HALF_PI (g/heading (tm/- p p')))]
    (q/line (v/+polar p r angle) (v/+polar p' r' angle))
    (q/line (v/-polar p r angle) (v/-polar p' r' angle))))

;; TODO: use a Cornu or clothoid spiral
(defn tangent-curve [c1 c2]
  (let [tightness 1.0
        spiral 0.2
        {p1 :p r1 :r} c1
        {p2 :p r2 :r} c2
        heading (g/heading (tm/- p1 p2))
        perp (+ tm/HALF_PI heading)]
    (swap! defo update :curves conj [c2 heading perp])
    [(v/+polar p1 (* (- 1.0 spiral) r1) (- perp tightness))
     (v/+polar p1 r1 perp)
     (v/+polar p1 (* (+ 1.0 spiral) r1) (+ perp tightness))
     (v/-polar p2 (* (+ 1.0 spiral) r2) (+ perp tightness))
     (v/-polar p2 r2 perp)
     (v/-polar p2 (* (- 1.0 spiral) r2) (- perp tightness))]))

(defn draw-points [curve]
  (q/fill 0)
  (doseq [[x y] curve]
    (cq/circle x y 2.0))
  (q/no-fill))

(defn plot [points]
  (q/push-style)
  (q/stroke-weight 0.8)
  (q/stroke 0.0 0.5 0.5)
  (q/begin-shape)
  (doseq [[x y] points]
    (cq/circle x y 2.0)
    (q/vertex x y))
  (q/end-shape)
  (q/pop-style))

(defn draw [{:keys [circles]}]
  (reset! defo {:c1 (first circles)
                :curves []})
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  (q/stroke-weight 1.2)
  (q/stroke 0)
  (doseq [c circles]
    (cq/circle c))

  (doseq [{:keys [parent] :as c1} circles
          :when parent
          :let [c2 (nth circles parent)
                curve (tangent-curve c2 c1)]]
    #_(tangent-lines c1 c2)

    #_(draw-points curve)
    (cq/draw-curve-path curve)
    #_(cq/draw-path curve))
  (plot (eq/clothoid 17.32 40 20 -1 0.0 (cq/rel-vec 0.5 0.5)))
  (plot (eq/clothoid 10 40 50 -1 Math/PI (cq/rel-vec 0.5 0.5)))
  (plot (eq/clothoid-from (eq/clothoid-A (cq/rel-h 0.15) 200) 200 50 -1
                          (* -0.15 Math/PI) (tm/+ (cq/rel-vec 0.5 0.2) (gv/vec2 (cq/rel-h 0.15) 0))))
  ;; Draw all the sibling tangents?
  (q/stroke-weight 0.5)
  #_(doseq [a [1 2]
            b [3 4]]
      (tangent-lines (nth circles a) (nth circles b))))

(defn page []
  [sketch/with-explanation
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:p "WIP experiment with connecting tangent clothoids between a pair of circles."]
   [debug/display defo]])

(sketch/definition stem-and-leaf
  {:created-at "2021-07-21"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
