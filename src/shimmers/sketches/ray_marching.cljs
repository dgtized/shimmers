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
            [thi.ng.geom.core :as g]
            [thi.ng.geom.polygon :as gp]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

;; Reference for future work:
;; https://legends2k.github.io/2d-fov/design.html
;; https://michaelwalczyk.com/blog-ray-marching.html
;; http://jamie-wong.com/2016/07/15/ray-marching-signed-distance-functions/#signed-distance-functions

(defonce ui-state
  (ctrl/state {:mode :ray-march
               :bounding-rectangle true
               :omnidirectional true
               :show-path false}))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:theta 0.0
   :mouse (cq/rel-vec 0.5 0.5)})

(defn update-state [state]
  (-> state
      (update :theta + 0.025)
      (update :mouse cq/mouse-last-position-clicked)))

(defn polar-project [p theta radius]
  (tm/+ p (v/polar radius theta)))

(defn circle-blob [[cx cy] rmin rmax dt]
  (gp/polygon2
   (for [angle (sm/range-subdivided tm/TWO_PI 10)]
     (let [xoff (+ (q/cos angle) 1)
           yoff (+ (q/sin angle) 1)
           r (q/map-range (q/noise xoff yoff dt) 0 1 rmin rmax)]
       (polar-project (gv/vec2 cx cy) angle r)))))

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
  (loop [depth 0 path []]
    (let [position (tm/+ from (v/polar depth angle))
          [close-a close-b] (apply min-key (fn [[a b]] (sdf-line position a b 1)) segments)
          dist (sdf-line position close-a close-b 1)]
      (cond
        (> depth 1000)
        [nil path]
        (< dist 0.1)
        [position path]
        :else
        (recur
         (+ depth dist)
         (conj path [position (* 2 dist)]))))))

(defn draw-ray [from hit path {:keys [show-path]}]
  (when show-path
    (q/stroke-weight 0.4)
    (q/stroke 0.0 0.5 0.5)
    (doseq [[c r] path]
      (cq/circle c r)))
  (when hit
    (let [inside? (g/contains-point? (cq/screen-rect) hit)]
      (q/stroke-weight (if inside? 0.8 0.33))
      (q/stroke (if inside? 0.33 0.66))
      (q/line from hit))))

(defn draw-shapes [shapes]
  (q/stroke 0.0)
  (q/stroke-weight 2.0)
  (doseq [shape shapes]
    (cq/draw-shape (g/vertices shape))))

(defn draw-state [{:keys [theta mouse]}]
  (q/background 1.0)
  (q/stroke 0.0)
  (q/stroke-weight 0.75)
  (q/no-fill)
  (let [{:keys [mode omnidirectional bounding-rectangle] :as ui-mode} @ui-state
        shapes (let [s (gen-shapes theta)]
                 (if bounding-rectangle
                   (conj s (cq/screen-rect 1.1))
                   s))
        segments (mapcat g/edges shapes)]
    (case mode
      :closest
      (doseq [angle (sm/range-subdivided tm/TWO_PI 200)]
        (let [ray [mouse (polar-project mouse angle (q/width))]]
          (when-let [intersection (closest-intersection ray segments)]
            (q/line mouse intersection))))
      :ray-march
      (if omnidirectional
        (doseq [angle (sm/range-subdivided tm/TWO_PI 45)
                :let [[hit path] (ray-march mouse angle segments)]]
          (draw-ray mouse hit path ui-mode))
        (let [angle (* theta 0.5)
              [hit path] (ray-march mouse angle segments)]
          (draw-ray mouse hit path ui-mode))))

    (draw-shapes shapes)))

(defn explanation []
  [:div
   [:p "In " [:em "closest"] " mode, rays are drawn from the selected origin to
   the closest segment along the path of the ray."]
   [:p "In " [:em "ray-march"] " mode, rays are drawn from the selected origin, but moving forward each step only as far as the closest segment to that point. The closest jump distance can be visualized if " [:em "closest surface radius"] " is enabled."]
   [:p "Click inside the canvas to place the ray origin."]])

(defn ui-controls []
  (ctrl/container
   (ctrl/change-mode ui-state [:ray-march :closest])
   (ctrl/checkbox ui-state "Include Bounding Rectangle in Shapes" [:bounding-rectangle])
   (when (= :ray-march (:mode @ui-state))
     [:div
      (ctrl/checkbox ui-state "Omnidirectional" [:omnidirectional])
      (ctrl/checkbox ui-state "Closest Surface Radius" [:show-path])])
   [explanation]))

(sketch/defquil ray-marching
  :created-at "2020-08-24"
  :on-mount #(ctrl/mount ui-controls)
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode framerate/mode])
