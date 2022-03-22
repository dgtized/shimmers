(ns shimmers.sketches.ray-marching
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.math.core :as sm]
            [shimmers.math.geometry.intersection :as isec]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

;; Reference for future work: https://legends2k.github.io/2d-fov/design.html

(defn setup []
  (q/frame-rate 30)
  {:theta 0.0})

(defn update-state [state]
  (update state :theta (fn [theta] (rem (+ theta 0.05) (* 2 Math/PI)))))

(defn circle-blob [[cx cy] rmin rmax]
  (for [angle (sm/range-subdivided tm/TWO_PI 10)]
    (let [dt (/ (q/frame-count) 50)
          xoff (+ (q/cos angle) 1)
          yoff (+ (q/sin angle) 1)
          r (q/map-range (q/noise xoff yoff dt) 0 1 rmin rmax)]
      (tm/+ (gv/vec2 cx cy) (v/polar r angle)))))

(defn polar-coord [theta radius x y]
  (tm/+ (gv/vec2 x y) (v/polar radius theta)))

(defn shape-segments
  "Convert vertices into a list of paired segments connecting each vertice in a loop."
  [vertices]
  (conj (partition 2 1 vertices) (list (last vertices) (first vertices))))

(defn closest-intersection [ray segments]
  ;; FIXME: slow, this is all pairs
  (->> segments
       ;; FIXME: why does segment-intersect order matter?
       (keep (fn [segment] (isec/segment-intersect segment ray)))
       (sort-by (fn [[sx sy]]
                  (let [[x y] (first ray)]
                    (q/dist x y sx sy))))
       first))

(defn center-origin []
  [(/ (q/width) 2)
   (/ (q/height) 2)])

(defn draw-state [{:keys [theta]}]
  (q/background 0)
  (q/stroke 255)
  (q/no-fill)
  (let [shapes [(circle-blob (polar-coord theta 50 100 100)
                             25 50)
                (circle-blob (polar-coord (+ theta 2) 25 400 400)
                             25 50)]
        segments (mapcat shape-segments shapes)]

    (doseq [angle (sm/range-subdivided tm/TWO_PI 200)]
      (let [origin (cq/mouse-position)
            [x y] origin
            ray [origin [(+ x (* 1000 (q/cos angle))) (+ y (* 1000 (q/sin angle)))]]]
        (when-let [intersection (closest-intersection ray segments)]
          (q/line origin intersection))))
    (doseq [shape shapes]
      (cq/draw-shape shape))))

(sketch/defquil ray-marching
  :created-at "2020-08-24"
  :size [500 500]
  :setup setup
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode framerate/mode])
