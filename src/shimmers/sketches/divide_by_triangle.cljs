(ns shimmers.sketches.divide-by-triangle
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/no-loop)
  {})

(defn sdf-line [p a b r]
  (let [pa (tm/- p a)
        ba (tm/- b a)
        h (tm/clamp01 (/ (tm/dot pa ba) (tm/dot ba ba)))]
    (- (tm/mag (tm/- pa (tm/* ba h))) r)))

(defn trail [[sx sy] shape]
  (let [{[w h] :size} (g/bounds shape)]
    (-> shape
        (g/scale-size (/ 1 tm/PHI))
        (g/translate
         (gv/vec2 (* sx 0.33 w) (* sy 1.2 h)))
        (geometry/rotate-around-centroid (* 0.05 sx)))))

(defn draw [_]
  (q/ellipse-mode :radius)
  (q/stroke 0.0 (dr/rand-nth [0.1 0.15 0.2]))
  (q/fill 0.0 0.05)
  (let [sign (dr/weighted {1 4 -1 1})
        bounds (g/translate (cq/screen-rect 1.4) (cq/rel-vec +0.0 (* sign 0.2)))
        angle (* sign 0.15)
        line (g/translate (gl/line2 (cq/rel-vec 0.20 0.55) (cq/rel-vec 0.45 0.55))
                          (gv/vec2 (cq/rel-w (dr/rand-nth [0.0 0.1 0.35 0.45])) 0))
        [a b] (:points line)
        height (gv/vec2 0 (cq/rel-h 0.03))
        rect (g/rotate (rect/rect (tm/+ a height) (tm/- b height)) angle)]
    (doseq [{[x y] :p [w h] :size} (g/subdivide bounds {:num 16})]
      (dotimes [_ (dr/weighted {25 5 40 1})]
        (let [wide (dr/chance 0.25)
              brush (rect/rect (+ x (dr/gaussian 0.0 (* 0.4 w)))
                               (+ y (dr/gaussian 0.0 (* 0.2 h)))
                               (if wide
                                 w
                                 (dr/random (* 0.2 w) (* 0.6 w)))
                               (if wide
                                 (dr/random (* 0.2 h) (* 0.6 h))
                                 h))]
          (when (> (sdf-line (g/centroid brush) a b (cq/rel-h 0.1)) 0)
            (qdg/draw (g/rotate brush angle))))))
    (q/fill 0.6 0.5 0.4 1.0)
    (q/no-stroke)
    (qdg/draw rect)
    (let [color (dr/rand-nth [[0.0 0.6 0.45 1.0]
                              [0.1 0.8 0.55 1.0]
                              [0.16 0.7 0.55 1.0]
                              [0.33 0.65 0.35 1.0]
                              [0.85 0.4 0.4 1.0]])]
      (apply q/fill color))

    (let [sx (dr/rand-nth [-1 1])
          dh (gv/vec2 (* sx (cq/rel-h 0.03)) (cq/rel-h 0.15))
          r (cq/rel-h 0.08)
          center (tm/mix a b 0.5)
          up (triangle/inscribed-equilateral (gc/circle (tm/- center dh) r) (- tm/HALF_PI))
          down (triangle/inscribed-equilateral (gc/circle (tm/+ center dh) r) tm/HALF_PI)]
      (doseq [t (take 7 (iterate (partial trail [sx -1])
                                 (g/rotate up angle)))]
        (qdg/draw t))
      (doseq [t (take 7 (iterate (partial trail [(- sx) 0.8])
                                 (g/rotate down angle)))]
        (qdg/draw t)))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:div.contained.explanation
    [:div.readable-width
     [:p "Genuary2023 Day 6 - Steal like an Artist"]
     [:p "Inspired by "
      [:a {:href "https://mobile.twitter.com/CodeAndWood/status/1611270290071232513"}
       "CodeAndWood's sketch"] ". Experimenting with varying the density of
     rectangle brushes and the compositional frame."]]]])

(sketch/definition divide-by-triangle
  {:created-at "2023-01-06"
   :tags #{:genuary2023}
   :type :quil}
  (ctrl/mount page "sketch-host"))
