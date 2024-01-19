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

(defn cube-wobble
  [[a0 f0 p0] [a1 f1 p1] t]
  (* a0 (eq/unit-sin
         (+ (* f0 t)
            (* a1 (eq/cube (Math/sin (+ p1 (* f1 t)))))
            p0))))

(defn duty-cycle [outer inner t]
  (tm/smoothstep* 0.25 0.75 (cube-wobble outer inner t)))

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
        rotation (* (/ eq/TAU 3)
                    (Math/cos (+ 1.7 (/ t 37)
                                 (* 0.33 tm/PHI (eq/cube (Math/sin (+ 0.3 (/ t 17))))))))
        spiral-rot (* (duty-cycle [1.0 0.11 0.1] [2.1 0.57 2.1] t)
                      (Math/sin (+ (/ t 23) 0.1 (Math/cos (+ 0.5 (* 0.37 t))))))
        duty-scale (duty-cycle [1.0 0.17 0.7] [tm/PHI 0.29 2.7] t)]
    (q/with-translation (cq/rel-vec 0.5 0.5)
      (q/with-rotation [rotation]
        (doseq [pos (hex/cube-spiral (gv/vec3) divs)
                :let [hex (hex/cube-hexagon pos r)
                      centroid (g/centroid hex)
                      [cx cy] centroid
                      d (/ (tm/mag centroid) mmag)
                      sqrt-d (Math/sqrt d)
                      scale-factor (Math/sin (* 0.02 (+ (* 0.55 t cx) (* 0.65 t cy))))
                      scale (+ (- 0.9 d) (* 0.2 scale-factor))
                      rot (* Math/PI (Math/cos (+ (* 0.5 t)
                                            (* 2 (Math/sin (* 0.5 (+ (/ t (+ 1 cx)) (/ t (+ 1 cy)))))))))]]
          (-> hex
              (g/scale-size scale)
              (g/rotate (* Math/PI sqrt-d spiral-rot))
              (g/scale (+ 1 (* 0.5 sqrt-d
                               duty-scale
                               (tm/smoothstep* 0.25 1.0 scale-factor))))
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
