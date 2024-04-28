(ns shimmers.sketches.helix
  (:require
   [clojure.math :as math]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn R [^double f ^double p ^double a ^double s]
  (v/polar a (* eq/TAU (+ (* s f) p))))

(defn O ^double [^double f ^double p ^double s]
  (math/sin (* eq/TAU (+ (* s f) p))))

(defn fractional []
  (+ (/ (dr/random-int 1 8)
        (dr/random-int 1 5))
     (dr/gaussian 0.0 0.1)))

(defn harmonic-loop [center radius a b c]
  (for [s (tm/norm-range 256)]
    (-> (gv/vec2)
        (tm/+ (R a s 0.50 s))
        (tm/+ (R b 0 0.35 s))
        (tm/+ (R c 0 0.15 s))
        (tm/* radius)
        (tm/+ center))))

(defn shapes [a b c]
  (let [pts (harmonic-loop (rv 0.5 0.5) (* 0.45 height) a b c)
        r (* 0.0066 height)]
    (for [[p q] (partition 2 1 pts)]
      (let [z (tm/- q p)
            delta (g/rotate z (/ eq/TAU 4))
            left (tm/+ p delta)
            right (tm/- p delta)
            line-delta (tm/normalize delta (- (tm/mag delta) r))]
        (csvg/group {}
          (gc/circle left r)
          (gc/circle right r)
          (gl/line2 (tm/+ p line-delta)
                    (tm/- p line-delta)))))))

(defn scene [a b c]
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.5}
    (shapes a b c)))

(defn page []
  (let [a (fractional)
        b (- (fractional))
        c (* -3 (fractional))]
    (fn []
      [sketch/with-explanation
       [:div.canvas-frame [scene a b c]]
       [view-sketch/generate :helix]
       [:div.readable-width]])))

(sketch/definition helix
  {:created-at "2024-04-27"
   :tags #{}
   :type :svg}
  (ctrl/mount page))
