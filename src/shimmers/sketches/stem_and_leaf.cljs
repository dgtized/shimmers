(ns shimmers.sketches.stem-and-leaf
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
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
    (q/line (tm/+ p (v/polar r angle)) (tm/+ p' (v/polar r' angle)))
    (q/line (tm/- p (v/polar r angle)) (tm/- p' (v/polar r' angle)))))

;; TODO: use a Cornu or clothoid spiral
;; https://pwayblog.com/2016/07/03/the-clothoid/ origin is the midpoint where r
;; is infinity and L is 0, and each side of the curve is one of the two circles.
;; https://math.stackexchange.com/questions/1785816/calculating-coordinates-along-a-clothoid-betwen-2-curves
;; https://etrr.springeropen.com/articles/10.1007/s12544-013-0119-8
(defn tangent-curve [c1 c2]
  (let [tightness 1.0
        spiral 0.2
        {p1 :p r1 :r} c1
        {p2 :p r2 :r} c2
        heading (g/heading (tm/- p1 p2))
        perp (+ tm/HALF_PI heading)]
    (swap! defo update :curves conj [c2 heading perp])
    [(tm/+ p1 (v/polar (* (- 1.0 spiral) r1) (- perp tightness)))
     (tm/+ p1 (v/polar r1 perp))
     (tm/+ p1 (v/polar (* (+ 1.0 spiral) r1) (+ perp tightness)))
     (tm/- p2 (v/polar (* (+ 1.0 spiral) r2) (+ perp tightness)))
     (tm/- p2 (v/polar r2 perp))
     (tm/- p2 (v/polar (* (- 1.0 spiral) r2) (- perp tightness)))]))

(defn draw-points [curve]
  (q/fill 0)
  (doseq [[x y] curve]
    (cq/circle x y 2.0))
  (q/no-fill))

(defn draw [{:keys [circles]}]
  (reset! defo {:c1 (first circles)
                :curves []})
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  (q/stroke-weight 1.2)
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
  ;; Draw all the sibling tangents?
  (q/stroke-weight 0.5)
  #_(doseq [a [1 2]
            b [3 4]]
      (tangent-lines (nth circles a) (nth circles b))))

(sketch/defquil stem-and-leaf
  :created-at "2021-07-21"
  :size [800 600]
  :on-mount #(debug/mount defo)
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
