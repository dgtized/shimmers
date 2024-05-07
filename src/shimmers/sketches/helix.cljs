(ns shimmers.sketches.helix
  (:require
   [clojure.math :as math]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.wobble :as mw :refer [R]]
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



(defn O ^double [^double f ^double p ^double s]
  (math/sin (* eq/TAU (+ (* s f) p))))

;; TODO use lcm to find single loop?
(defn harmonic-loop [center radius {:keys [n-points a a-osc b b-osc c c-osc]}]
  (for [s (tm/norm-range n-points)]
    (-> (gv/vec2)
        (tm/+ (R a (* 0.2 (O a-osc 0 s)) 0.65 s))
        (tm/+ (R b (* 0.6 (O b-osc 0 (- 1.0 s))) 0.30 s))
        (tm/+ (R c (* 0.8 (O c-osc 0 s)) 0.05 s))
        (tm/* radius)
        (tm/+ center))))

(defn shapes [{:keys [n-points angle-osc size-osc] :as params}]
  (let [pts (harmonic-loop (rv 0.5 0.5) (* 0.48 height) params)
        r (* 0.0033 height)]
    (for [[[p q] s] (mapv vector
                          (partition 2 1 pts)
                          (tm/norm-range n-points))]
      (let [z (tm/- q p)
            angle (* Math/PI (O angle-osc (math/sin (* s r)) s))
            delta  (tm/normalize (g/rotate z (+ angle (/ eq/TAU 4)))
                                 (+ (* 3 tm/PHI r) (* 3 r (O size-osc (* 0.1 angle) s))))
            line-delta (tm/normalize delta (- (tm/mag delta) r))]
        (csvg/group {}
          (gc/circle (tm/+ p delta) r)
          (gc/circle (tm/- p delta) r)
          (gl/line2 (tm/+ p line-delta)
                    (tm/- p line-delta)))))))

(defn scene [params]
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.5}
    (shapes params)))

(defn page []
  (let [a (dr/weighted-by #(- 11 %) (range 1 11))
        b (dr/weighted-by #(- 13 %) (range 1 13))
        params
        {:n-points 2048
         :a (* (dr/weighted {-1 1 1 6}) a)
         :b (* (dr/weighted {-1 6 1 1}) b)
         :c (dr/weighted
             [[(* (min a b) (dr/random-int 1 6)) 2.0]
              [(sm/lcm a b) 1.0]
              [(sm/gcd a b) 1.0]])
         :a-osc (* (dr/random-sign) (dr/random-int 4))
         :b-osc (* (dr/random-sign) (dr/random-int 6))
         :c-osc (* (dr/random-sign) (dr/random-int 12))
         :angle-osc (dr/random-int 6 24)
         :size-osc (dr/random-int 6 24)}]
    (fn []
      [sketch/with-explanation
       [:div.canvas-frame [scene params]]
       [view-sketch/generate :helix]
       [:div.readable-width
        (debug/pre-edn params)]])))

(sketch/definition helix
    {:created-at "2024-04-27"
     :tags #{}
     :type :svg}
  (ctrl/mount page))
