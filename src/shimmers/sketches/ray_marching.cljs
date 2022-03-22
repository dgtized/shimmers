(ns shimmers.sketches.ray-marching
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.core :as sm]
            [shimmers.math.geometry.intersection :as isec]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

;; Reference for future work: https://legends2k.github.io/2d-fov/design.html
;; and  https://michaelwalczyk.com/blog-ray-marching.html

(defonce ui-state (ctrl/state {:mode :mouse}))

(defn setup []
  {:theta 0.0
   :mouse (gv/vec2)})

(defn update-state [state]
  (-> state
      (update :theta + 0.025)
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

(defn gen-shapes [theta]
  (let [r-min (cq/rel-w 0.05)
        r-max (cq/rel-w 0.18)]
    [(circle-blob (polar-project (cq/rel-vec 0.3 0.3) theta (cq/rel-w 0.04))
                  r-min r-max
                  (* theta 0.20))
     (circle-blob (polar-project (cq/rel-vec 0.7 0.2) (* 0.1 theta) (cq/rel-w 0.025))
                  (* 0.5 r-min) (* 0.5 r-max)
                  (* theta 0.1))
     (circle-blob (polar-project (cq/rel-vec 0.8 0.4) (* 0.1 theta) (cq/rel-w 0.01))
                  (* 0.5 r-min) (* 0.5 r-max)
                  (* theta 0.1))
     (circle-blob (polar-project (cq/rel-vec 0.6 0.7) (+ theta 2) (cq/rel-w 0.08))
                  r-min r-max
                  (* theta 0.40))]))

;; https://www.iquilezles.org/www/articles/distfunctions/distfunctions.htm
(defn sdf-line [p a b r]
  (let [pa (tm/- p a)
        ba (tm/- b a)
        h (tm/clamp01 (/ (tm/dot pa ba) (tm/dot ba ba)))]
    (- (tm/mag (tm/- pa (tm/* ba h))) r)))

(defn ray-march [from angle segments]
  (loop [depth 0]
    (let [position (tm/+ from (v/polar depth angle))
          [close-a close-b] (apply min-key (fn [[a b]] (sdf-line position a b 1)) segments)
          dist (sdf-line position close-a close-b 1)]
      (apply q/point position)
      (cq/circle position (* 2 dist))
      (cond
        (> depth 1000)
        nil
        (< dist 0.1)
        position
        :else
        (recur (+ depth dist))))))

(defn draw-state [{:keys [theta mouse]}]
  (q/background 0)
  (q/stroke 255)
  (q/no-fill)
  (let [shapes (gen-shapes theta)
        segments (mapcat shape-segments shapes)]
    (case (:mode @ui-state)
      :mouse
      (doseq [angle (sm/range-subdivided tm/TWO_PI 200)]
        (let [ray [mouse (polar-project mouse angle (q/width))]]
          (when-let [intersection (closest-intersection ray segments)]
            (q/line mouse intersection))))
      :ray-march
      (let [from (cq/rel-vec 0.25 0.75)
            angle (+ (mod (* 0.25 theta) tm/PI) (* 1.25 tm/PI))]
        (when-let [intersection (ray-march from angle segments)]
          (q/line from intersection))))
    (doseq [shape shapes]
      (cq/draw-shape shape))))

(defn ui-controls []
  (ctrl/container
   (ctrl/change-mode ui-state [:mouse :ray-march])))

(sketch/defquil ray-marching
  :created-at "2020-08-24"
  :on-mount #(ctrl/mount ui-controls)
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode framerate/mode])
