(ns shimmers.sketches.countdown
  (:require
   [shimmers.common.ui.canvas :as canvas]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.math.hexagon :as hex]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn setup [_cv]
  {})

(defn update-state [state _dims _ms]
  state)

(defn draw [_cv ctx [width height] ms]
  (let [seconds (* 0.001 ms)
        t (* 0.08 seconds)
        radius 25
        hexes (hex/cube-spiral (gv/vec3) 6)
        center (gv/vec2 (* 0.5 width) (* 0.5 height))
        corner (tm/mag (tm/- (gv/vec2 width height) center))]
    (canvas/clear ctx width height)
    (doseq [[i hex] (map-indexed vector hexes)
            :let [hex-pos (hex/cube-hexagon hex radius)
                  r (tm/mag (:p hex-pos))
                  pr (/ r corner)
                  pos (tm/+ center (:p hex-pos))
                  pct (/ (float i) (count hexes))
                  rot (+ t (* 0.5 (g/heading (:p hex-pos))))
                  o-t0 (* eq/TAU (+ (Math/sin (+ t 0.02 (* pct (Math/sin (* 0.4 t))))) rot))
                  o-t1 (* eq/TAU (+ (Math/sin (- t 0.03 (* pct (Math/sin (* 0.5 t))))) rot))
                  i-t0 (* eq/TAU (+ (Math/sin (+ t 0.04 (* pct (Math/sin (* 0.6 t))))) rot))
                  i-t1 (* eq/TAU (+ (Math/sin (- t 0.01 (* pct (Math/sin (* 0.3 t))))) rot))]]
      (canvas/line-width ctx (* 10.0 (+ 0.1 pr)))
      (canvas/clockwise-arc ctx pos
                            (- radius 15
                               (* 6 (Math/sin (+ (* Math/PI pr) (* 10 t)))))
                            o-t0 o-t1)
      (.stroke ctx)
      (canvas/line-width ctx (* 4.0 (+ 0.1 pr)))
      ;; min/max t0/t1 to remove clip jump
      (canvas/clockwise-arc ctx pos
                            (- radius 20
                               (* 3 (Math/sin (+ (* Math/PI pr) (* 15 t)))))
                            (min i-t0 i-t1) (max i-t0 i-t1))
      (.stroke ctx)))
  ctx)

(defn page []
  (let [{:keys [canvas-state attributes]}
        (canvas/make-state
         {:width 800
          :height 600
          :setup #'setup
          :update #'update-state
          :draw #'draw}
         {:width-pct 0.7})]
    (fn []
      [:div
       [canvas/canvas-frame attributes canvas-state canvas/animate-frame]])))

(sketch/definition countdown
    {:created-at "2024-02-11"
     :tags #{}
     :type :canvas}
  (ctrl/mount (page)))
