(ns shimmers.sketches.remaining
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [a (dr/weighted-by #(- 11 %) (range 1 11))
        b (dr/weighted-by #(- 13 %) (range 1 13))]
    {:params
     {:n-points 200
      :a (* (dr/weighted {-1 1 1 6}) a)
      :b (* (dr/weighted {-1 6 1 1}) b)
      :c (dr/weighted
          [[(* (min a b) (dr/random-int 1 6)) 2.0]
           [(sm/lcm a b) 1.0]
           [(sm/gcd a b) 1.0]])
      :a-osc (* (dr/random-sign) (dr/random-int 5))
      :b-osc (* (dr/random-sign) (dr/random-int 7))
      :c-osc (* (dr/random-sign) (dr/random-int 13))
      :phase 0
      :angle-osc (dr/random-int 6 24)
      :size-osc (dr/random-int 6 24)}}))

(defn R [^double f ^double p ^double a ^double s]
  (v/polar a (* eq/TAU (+ (* s f) p))))

(defn O ^double [^double f ^double p ^double s]
  (math/sin (* eq/TAU (+ (* s f) p))))

(defn harmonic-loop [center radius {:keys [n-points a a-osc b b-osc c c-osc phase]}]
  (for [s (tm/norm-range n-points)]
    (let [s (+ s phase)]
      (-> (gv/vec2)
          (tm/+ (R a (* 0.2 (O a-osc 0 s)) 0.65 s))
          (tm/+ (R b (* 0.6 (O b-osc 0 (- 1.0 s))) 0.30 s))
          (tm/+ (R c (* 0.8 (O c-osc 0 s)) 0.05 s))
          (tm/* radius)
          (tm/+ center)))))

(defn update-state [state]
  (update-in state [:params :phase] + (* 0.00025 (dr/random))))

(defn draw [{:keys [params]}]
  (q/stroke-weight (dr/random 0.1 0.6))
  (q/stroke 0.0 (dr/random 0.15 0.25))
  (q/no-fill)
  (let [t (/ (q/millis) 1000.0)]
    (if (> t 2.0)
      (q/no-loop)
      (let [height (q/height)
            {:keys [n-points angle-osc size-osc phase]} params
            pts (harmonic-loop (cq/rel-vec 0.5 0.5) (* 0.48 height) params)
            r (* 0.0075 height)]
        (doseq [[[p q] s] (mapv vector
                                (partition 2 1 pts)
                                (tm/norm-range n-points))]
          (let [z (tm/- q p)
                angle (* Math/PI (O angle-osc (math/sin (+ phase (* s r))) s))
                delta  (tm/normalize (g/rotate z (+ angle (/ eq/TAU 4)))
                                     (+ (* 2 tm/PHI r) (* 2 r (O size-osc (* 0.2 angle) s))))
                line-delta (tm/normalize delta (- (tm/mag delta) r))]
            (cq/circle (tm/+ p delta) r)
            (cq/circle (tm/- p delta) r)
            (q/line (tm/+ p line-delta)
                    (tm/- p line-delta))))))))

(defn page []
  [:div
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])])

(sketch/definition remaining
  {:created-at "2024-05-02"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
