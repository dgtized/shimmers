(ns shimmers.sketches.hexpansion
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.hexagon :as hex]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn seconds []
  (/ (q/millis) 1000.0))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t (seconds)})

(defn update-state [state]
  (assoc state :t (seconds)))

(defn draw [{:keys [t]}]
  (q/background 1.0)
  (q/stroke-weight 1.0)
  (q/stroke 0.0 1.0)
  (q/fill 1.0 0.2)
  (let [divs 12
        r (+ (/ (* 0.4 (q/height)) (* 2 divs)) (* (/ 3 divs) (Math/sin (* 0.75 t))))
        mmag (tm/mag (cq/rel-vec 0.5 0.5))
        rotation (* (/ eq/TAU 6) (Math/cos (+ (/ t 29) 0.5 (* tm/PHI (eq/cube (Math/sin (* 0.33 t)))))))
        spiral-rot (* 0.66 Math/PI (Math/sin (+ (/ t 23) 0.1 (Math/cos (+ 0.5 (* 0.37 t))))))]
    (q/with-translation (cq/rel-vec 0.5 0.5)
      (q/with-rotation [rotation]
        (doseq [pos (hex/cube-spiral (gv/vec3) divs)
                :let [hex (hex/cube-hexagon pos r)
                      centroid (g/centroid hex)
                      [cx cy] centroid
                      d (/ (tm/mag centroid) mmag)
                      scale-factor (Math/sin (* 0.02 (+ (* 0.55 t cx) (* 0.65 t cy))))
                      scale (+ (- 0.9 d) (* 0.2 scale-factor))
                      rot (* Math/PI (Math/cos (+ (* 0.5 t)
                                            (* 2 (Math/sin (* 0.5 (+ (/ t (+ 1 cx)) (/ t (+ 1 cy)))))))))]]
          (-> hex
              (g/scale-size scale)
              (g/rotate (* (Math/sqrt d)
                           (tm/smoothstep* 0.33 1.0 (abs spiral-rot))
                           spiral-rot))
              (g/scale (+ 1 (* (Math/sqrt d) 0.5
                               (tm/smoothstep* 0.33 1.0 scale-factor))))
              (g/vertices 6)
              (gp/polygon2)
              (geometry/rotate-around-centroid rot)
              (g/vertices)
              cq/draw-shape))))))

(defn page []
  [sketch/with-explanation
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:div
    [:h4 "Genuary 2024 - Day 10 - Hexagons"]]])

(sketch/definition hexpansion
  {:created-at "2024-01-14"
   :tags #{:genuary2024}
   :type :quil}
  (ctrl/mount page))
