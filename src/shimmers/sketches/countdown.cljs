(ns shimmers.sketches.countdown
  (:require
   [shimmers.common.ui.canvas :as canvas]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.math.hexagon :as hex]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [shimmers.math.equations :as eq]))

(defn setup [_cv]
  {})

(defn update-state [state _dims _ms]
  state)

(defn draw [_cv ctx [width height] ms]
  (let [t (* 0.0005 ms)
        radius 30
        hexes (hex/cube-spiral (gv/vec3) 5)
        center (gv/vec2 (* 0.5 width) (* 0.5 height))]
    (canvas/clear ctx width height)
    (canvas/line-width ctx 4.0)
    (doseq [[i hex] (map-indexed vector hexes)
            :let [hex-pos (hex/cube-hexagon hex radius)
                  pos (tm/+ center (:p hex-pos))
                  pct (/ (float i) (count hexes))]]
      (canvas/clockwise-arc ctx pos
                            (- radius 4)
                            (mod (* eq/TAU (+ t i)) eq/TAU)
                            (mod (* eq/TAU (+ t (* 2 pct)) ) eq/TAU))
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
