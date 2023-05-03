(ns shimmers.sketches.reflections
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.types :refer [Circle2]]
   [thi.ng.geom.vector :as gv]))

(defn reflect-identity [box p]
  (g/unmap-point box p))

(defn reflect-x [box [x y]]
  (g/unmap-point box (gv/vec2 (- 1.0 x) y)))

(defn reflect-y [box [x y]]
  (g/unmap-point box (gv/vec2 x (- 1.0 y))))

(defn reflect-xy [box [x y]]
  (g/unmap-point box (gv/vec2 (- 1.0 x) (- 1.0 y))))

(defn shapes [t]
  (let [dt 0.01]
    [(gc/circle (+ 0.5 (* 0.2 (Math/sin (* 3 t)))) 0.4 0.2)
     (gc/circle (+ 0.5 (* 0.2 (Math/sin (* 3 (+ t dt))))) 0.4 0.2)
     (g/translate (gc/circle 0.5 0.5 0.12) (v/polar 0.3 (* 4 (- t dt))))
     (g/translate (gc/circle 0.5 0.5 0.11) (v/polar 0.3 (* 4 t)))
     (g/translate (gc/circle 0.5 0.5 0.10) (v/polar 0.3 (* 4 (+ t dt))))
     (gl/line2 (gv/vec2 0.1 0.1) (gv/vec2 0.9 (+ 0.3 (* 0.2 (Math/sin (* 2.5 t))))))
     (gl/line2 (gv/vec2 0.1 0.1) (gv/vec2 0.9 (+ 0.3 (* 0.2 (Math/sin (* 2.5 (+ t dt)))))))
     (gl/line2 (gv/vec2 0.1 0.5) (gv/vec2 0.9 0.9))
     (gl/line2 (gv/vec2 (+ 0.5 (* 0.35 (Math/sin (* 0.3 t)))) 0.1)
               (gv/vec2 (+ 0.5 (* 0.3 (Math/sin (* 0.4 t)))) 0.9))
     (gl/line2 (gv/vec2 (+ 0.5 (* 0.35 (Math/sin (* 0.3 (- t dt))))) 0.1)
               (gv/vec2 (+ 0.5 (* 0.3 (Math/sin (* 0.4 (- t dt))))) 0.9))]))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0})

(defn update-state [state]
  (update state :t + 0.0025))

(defn draw [{:keys [t]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (let [height (q/height)
        width (q/width)
        ref-shape (-> (cq/rel-vec 0.5 0.5)
                      (gc/circle (* 0.25 height))
                      (triangle/inscribed-equilateral (/ t 12)))
        [rx ry] (g/point-at ref-shape (mod t 1.0))
        box (rect/rect 0 0 rx ry)
        reflections [[reflect-identity box]
                     [reflect-x (rect/rect rx 0 (- width rx) ry)]
                     [reflect-y (rect/rect 0 ry rx (- height ry))]
                     [reflect-xy (rect/rect rx ry (- width rx) (- height ry))]]]
    (doseq [s (shapes t)]
      (cond (instance? Circle2 s)
            (let [{:keys [p r]} s]
              (doseq [[reflect box] reflections]
                (cq/circle (reflect box p) (* r (g/height box)))))
            :else
            (doseq [[reflect box] reflections]
              (q/begin-shape)
              (doseq [v (g/vertices s)]
                (apply q/vertex (reflect box v)))
              (q/end-shape))))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:div.contained.explanation
    [:p "Genuary 2023 Day 16 - Reflections on Reflections"]
    [:p.readable-width "Reflect the animated scene in the top left over both x,
   y, and x/y axis. However, to shake things up a little, move the origin point
   specifying each axis over time."]]])

(sketch/definition reflections
  {:created-at "2023-01-16"
   :tags #{:genuary2023}
   :type :quil}
  (ctrl/mount page))
