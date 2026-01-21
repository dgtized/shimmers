(ns shimmers.sketches.helix
  (:require
   [clojure.math :as math]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.wobble :as mw :refer [O R]]
   [shimmers.model.harmonics :as harm]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

;; TODO use lcm to find single loop?
(defn harmonic-loop [center radius {:keys [n-points a a-osc b b-osc c c-osc]}]
  (for [s (tm/norm-range n-points)]
    (-> (gv/vec2)
        (tm/+ (R a (* 0.2 (O a-osc 0 s)) 0.65 s))
        (tm/+ (R b (* 0.6 (O b-osc 0 (- 1.0 s))) 0.30 s))
        (tm/+ (R c (* 0.8 (O c-osc 0 s)) 0.05 s))
        (tm/* radius)
        (tm/+ center))))

(defn shapes [{:keys [n-points angle-osc size-osc radius-osc] :as params}]
  (let [pts (harmonic-loop (rv 0.5 0.5) (* 0.48 height) params)
        radius (* 0.0033 height)]
    (for [[[p q] s] (mapv vector
                          (partition 2 1 pts)
                          (tm/norm-range n-points))]
      (let [r (+ radius (* 0.35 radius (O radius-osc 0 s)))
            z (tm/- q p)
            angle (* tm/PI (O angle-osc (math/sin (* s radius)) s))
            delta  (tm/normalize (g/rotate z (+ angle (/ eq/TAU 4)))
                                 (+ (* 3 tm/PHI r) (* 3 r (O size-osc (* 0.1 angle) s))))
            line-delta (tm/normalize delta (- (tm/mag delta) r))]
        (csvg/group {}
          (gc/circle (tm/+ p delta) r)
          (gc/circle (tm/- p delta) r)
          (gl/line2 (tm/+ p line-delta)
                    (tm/- p line-delta)))))))

(defn scene [{:keys [scene-id params]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.5}
    (shapes params)))

(defn parameters []
  (let [[a b c] (harm/abc)]
    {:n-points 2048
     :a a
     :b b
     :c c
     :a-osc (* (dr/random-sign) (dr/random-int 5))
     :b-osc (* (dr/random-sign) (dr/random-int 7))
     :c-osc (* (dr/random-sign) (dr/random-int 13))
     :angle-osc (dr/random-int 6 24)
     :size-osc (dr/random-int 6 24)
     :radius-osc (dr/random-int 6 24)}))

(sketch/definition helix
  {:created-at "2024-04-27"
   :tags #{}
   :type :svg}
  (ctrl/mount
   (usvg/let-page sketch-args
                  parameters
                  (fn [{:keys [params]}] (debug/pre-edn params))
                  scene)))
