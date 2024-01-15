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
        mmag (tm/mag (cq/rel-vec 0.5 0.5))]
    (q/with-translation (cq/rel-vec 0.5 0.5)
      (q/with-rotation [(* Math/PI (Math/cos (+ (* 0.2 t) (* tm/PHI (eq/cube (Math/sin (* 0.1 t)))))))]
        (doseq [pos (hex/cube-spiral (gv/vec3) divs)
                :let [hex (hex/cube-hexagon pos r)
                      centroid (g/centroid hex)
                      [cx cy] centroid
                      scale (+ (- 0.9 (/ (tm/mag centroid) mmag))
                               (* 0.2 (Math/sin (* 0.02 (+ (* 0.65 t cx) (* 0.75 t cy))))))
                      rot (* Math/PI (Math/cos (+ (* 0.5 t)
                                            (* 2 (Math/sin (* 0.5 (+ (/ t (+ 1 cx)) (/ t (+ 1 cy)))))))))]]
          (-> hex
              (g/scale-size scale)
              (g/scale (+ 1.0 (* 0.66 scale)))
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
