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
  {:theta 0.0
   :mouse (gv/vec2)})

(defn update-state [state]
  (-> state
      (update :theta (fn [theta] (rem (+ theta 0.025) (* 2 Math/PI))))
      (assoc :mouse (cq/mouse-position))))

(defn polar-project [p theta radius]
  (tm/+ p (v/polar radius theta)))

(defn circle-blob [[cx cy] rmin rmax dt]
  (for [angle (sm/range-subdivided tm/TWO_PI 10)]
    (let [xoff (+ (q/cos angle) 1)
          yoff (+ (q/sin angle) 1)
          r (q/map-range (q/noise xoff yoff dt) 0 1 rmin rmax)]
      (polar-project (gv/vec2 cx cy) angle r))))

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

(defn draw-state [{:keys [theta mouse]}]
  (q/background 0)
  (q/stroke 255)
  (q/no-fill)
  (let [dt (/ (q/frame-count) 150)
        r-min (cq/rel-w 0.05)
        r-max (cq/rel-w 0.18)
        shapes [(circle-blob (polar-project (cq/rel-vec 0.3 0.3) theta (cq/rel-w 0.04))
                             r-min r-max
                             dt)
                (circle-blob (polar-project (cq/rel-vec 0.7 0.7) (+ theta 2) (cq/rel-w 0.08))
                             r-min r-max
                             dt)]
        segments (mapcat shape-segments shapes)]

    (doseq [angle (sm/range-subdivided tm/TWO_PI 200)]
      (let [ray [mouse (polar-project mouse angle (q/width))]]
        (when-let [intersection (closest-intersection ray segments)]
          (q/line mouse intersection))))
    (doseq [shape shapes]
      (cq/draw-shape shape))))

(sketch/defquil ray-marching
  :created-at "2020-08-24"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode framerate/mode])
