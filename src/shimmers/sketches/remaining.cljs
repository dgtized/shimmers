(ns shimmers.sketches.remaining
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.screen :as screen]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce ui-state (ctrl/state {:screen-size "900x600"}))
(defonce defo (debug/state {}))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/ellipse-mode :radius)
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
      :size-osc (dr/random-int 6 24)
      :radius-osc (dr/random-int 8 32)}}))

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
  (update-in state [:params :phase] +
             (-> (dr/gaussian 2.0 0.5)
                 (tm/clamp 0.5 5.0)
                 (/ 30000.0))))

(defn draw [{:keys [params]}]
  (q/stroke-weight (tm/clamp (dr/gaussian 0.225 0.05) 0.1 0.6))
  (q/stroke 0.0 (tm/clamp (dr/gaussian 0.2 0.06) 0.1 0.6))
  (q/no-fill)
  (reset! defo params)
  (let [t (/ (q/millis) 1000.0)]
    (if (> t 2.0)
      (q/no-loop)
      (let [height (q/height)
            {:keys [n-points angle-osc radius-osc size-osc phase]} params
            pts (harmonic-loop (cq/rel-vec 0.5 0.5) (* 0.45 height) params)
            radius (* 0.0075 height)]
        (doseq [[[p q] s] (mapv vector
                                (partition 2 1 pts)
                                (tm/norm-range n-points))]
          (let [z (tm/- q p)
                r (+ radius (* 0.2 radius (O radius-osc (* 0.25 phase) s)))
                angle (* Math/PI (O angle-osc (* Math/PI (math/sin (+ (* s radius) phase))) s))
                delta  (tm/normalize (g/rotate z (+ angle (/ eq/TAU 4)))
                                     (+ (* 2 tm/PHI r) (* 2 r (O size-osc (+ 2.0 (* 0.2 angle)) s))))
                line-delta (tm/normalize delta (- (tm/mag delta) r))]
            (cq/circle (tm/+ p delta) r)
            (cq/circle (tm/- p delta) r)
            (q/line (tm/+ p line-delta)
                    (tm/- p line-delta))))))))

(defn page []
  [sketch/with-explanation
   (sketch/component
     :size (screen/parse-size (:screen-size @ui-state))
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   [:div.flexcols
    [ctrl/container
     [ctrl/dropdown ui-state "Screen Size" [:screen-size]
      (screen/sizes)
      {:on-change #(view-sketch/restart-sketch :remaining)}]]
    [:div [:p]
     [debug/display defo]]]])

(sketch/definition remaining
  {:created-at "2024-05-02"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
