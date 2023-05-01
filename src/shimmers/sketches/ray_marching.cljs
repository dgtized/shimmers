(ns shimmers.sketches.ray-marching
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.core :as sm]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.intersection :as isec]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; Reference for future work:
;; https://legends2k.github.io/2d-fov/design.html
;; https://michaelwalczyk.com/blog-ray-marching.html
;; http://jamie-wong.com/2016/07/15/ray-marching-signed-distance-functions/#signed-distance-functions

(defonce ui-state
  (ctrl/state {:animated true
               :mode :ray-march
               :scene :shapes
               :bounding-rectangle true
               :omnidirectional true
               :visible-shapes true
               :show-path false}))

(def defo (ctrl/state {}))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:theta 0.0
   :mouse (cq/rel-vec 0.5 0.5)})

(defn update-state [state]
  (-> (if (:animated @ui-state)
        (update state :theta + 0.025)
        state)
      (update :mouse cq/mouse-last-position-clicked)))

;; How to make an SDF of a blob from noise directly?
(defn circle-blob [[cx cy] rmin rmax dt]
  (gp/polygon2
   (for [angle (sm/range-subdivided tm/TWO_PI 10)]
     (let [xoff (+ (q/cos angle) 1)
           yoff (+ (q/sin angle) 1)
           r (q/map-range (q/noise xoff yoff dt) 0 1 rmin rmax)]
       (v/+polar (gv/vec2 cx cy) r angle)))))

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
    [(circle-blob (v/+polar (cq/rel-vec 0.3 0.3) (cq/rel-w 0.04) theta)
                  r-min r-max
                  (* theta 0.20))
     (circle-blob (v/+polar (cq/rel-vec 0.7 0.2) (cq/rel-w 0.025) (* 0.1 theta))
                  (* 0.5 r-min) (* 0.5 r-max)
                  (* theta 0.1))
     (circle-blob (v/+polar (cq/rel-vec 0.8 0.4) (cq/rel-w 0.01) (* 0.1 theta))
                  (* 0.5 r-min) (* 0.5 r-max)
                  (* theta 0.1))
     (circle-blob (v/+polar (cq/rel-vec 0.6 0.7) (cq/rel-w 0.08) (+ theta 2))
                  r-min r-max
                  (* theta 0.40))
     (g/translate (cq/screen-rect 0.2) (cq/rel-vec -0.35 0.35))]))

(defn gen-boxes [_]
  [(g/translate (cq/screen-rect 0.3) (cq/rel-vec 0.1 -0.2))
   (g/translate (cq/screen-rect 0.2) (cq/rel-vec -0.25 0.1))
   (g/translate (cq/screen-rect 0.15) (cq/rel-vec 0.2 0.2))])

;; https://www.iquilezles.org/www/articles/distfunctions/distfunctions.htm
(defn sdf-line [p a b r]
  (let [pa (tm/- p a)
        ba (tm/- b a)
        h (tm/clamp01 (/ (tm/dot pa ba) (tm/dot ba ba)))]
    (- (tm/mag (tm/- pa (tm/* ba h))) r)))

;; ~55% of profiled total time instead of ~59.5% for sdf-line, but may also
;; reduce GC as frame-rate jumps. Main problem is N^2 check for closest segment though
;; Disabled for now to test against ADVANCED_COMPILATION of original.
(defn inline-sdf-line [[px py] [ax ay] [bx by] r]
  (let [pax (- px ax)
        pay (- py ay)
        bax (- bx ax)
        bay (- by ay)
        h (let [t (/ (+ (* pax bax) (* pay bay))
                     (+ (* bax bax) (* bay bay)))]
            (cond (< t 0) 0.0
                  (> t 1) 1.0
                  :else t))]
    (- (Math/sqrt (+ (eq/sqr (- pax (* bax h)))
                     (eq/sqr (- pay (* bay h)))))
       r)))

(defn world-map [segments]
  (fn [position]
    (let [[close-a close-b] (apply min-key (fn [[a b]] (sdf-line position a b 1)) segments)]
      (sdf-line position close-a close-b 1))))

(defn ray-march [from angle world]
  (loop [depth 0 path []]
    (let [position (v/+polar from depth angle)
          dist (world position)]
      (cond
        (> depth 1000)
        [nil path]
        (< dist 0.1)
        [position path]
        :else
        (recur
         (+ depth dist)
         (conj path [position dist]))))))

(defn estimate-normal [world epsilon position]
  (tm/normalize
   (gv/vec2 (- (world (tm/+ position (gv/vec2 epsilon 0)))
               (world (tm/- position (gv/vec2 epsilon 0))))
            (- (world (tm/+ position (gv/vec2 0 epsilon)))
               (world (tm/- position (gv/vec2 0 epsilon)))))))

;; in https://graphicscodex.courses.nvidia.com/app.html?page=_rn_rayMrch,
;; section 6 mentions computing a gradient of the SDF directly.
;; other notes: https://math.stackexchange.com/questions/13261/how-to-get-a-reflection-vector
;; https://antoinefortin.ca/uncategorized/simple-ray-marching-reflection/
(defn reflect-ray-march [max-distance max-steps from angle world]
  (loop [distance 0
         steps 0
         position from
         angle angle
         path []]
    (let [epsilon 0.1
          dist (world position)]
      (cond (or (> distance max-distance) (> steps max-steps))
            [position path]
            (<= dist epsilon)
            (let [facing-normal (estimate-normal world epsilon position)
                  reflection (g/reflect (tm/- (v/polar 1 angle)) facing-normal)
                  angle' (g/heading reflection)]
              (swap! defo update :reflection (fnil conj [])
                     {:p position
                      :dist dist
                      :ray (v/polar 1 angle)
                      :normal facing-normal
                      :reflect reflection})
              (recur (+ distance epsilon)
                     (inc steps)
                     (v/+polar position epsilon angle')
                     angle'
                     (conj path [position dist])))
            :else
            (recur (+ distance dist)
                   (inc steps)
                   (v/+polar position dist angle)
                   angle
                   (conj path [position dist]))))))

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
    (let [vertices (g/vertices shape)]
      (cq/draw-shape vertices)
      (doseq [[a b] (g/edges shape)
              :let [mid (tm/mix a b 0.5)]]
        (q/line mid (tm/+ mid (tm/normalize (g/normal (tm/- a b)) 3)))))))

(defn ray-from [position p]
  [position (tm/+ position (tm/* (tm/normalize (tm/- p position)) (q/width)))])

(defn closest-segment [ray segments]
  ;; FIXME: slow, this is all pairs
  (->> segments
       ;; FIXME: why does segment-intersect order matter?
       (keep (fn [segment] (let [hit (isec/segment-intersect segment ray)]
                            [hit segment])))
       (sort-by (fn [[[sx sy] _]]
                  (let [[x y] (first ray)]
                    (q/dist x y sx sy))))
       first))

(defn sweep-segment-hits [position segments]
  (->> (for [angle (sm/range-subdivided tm/TWO_PI 60)
             :let [ray [position (v/+polar position (q/width) angle)]
                   [hit segment] (closest-segment ray segments)]
             :when hit]
         [hit segment])
       (sort-by (fn [[hit _]] (g/heading (tm/- hit position))))))

(defn segment-hits [position segments]
  (->> segments
       (mapcat (fn [segment]
                 (for [p segment
                       :let [ray (ray-from position p)
                             [hit hit-seg] (closest-segment ray segments)]
                       :when hit]
                   [hit hit-seg])))
       (sort-by (fn [[hit _]] (g/heading (tm/- hit position))))))

(defn grouped-segment-hits [position segments]
  (->> (segment-hits position segments)
       (partition-by second)
       (mapcat (fn [group]
                 (map first [(first group) (last group)])))))

(defn ordered-points [position segments]
  (->> segments
       (mapcat (fn [segment]
                 (let [[a b] segment] [[a segment] [b segment]])))
       (sort-by (fn [[p _]] (g/heading (tm/- p position))))))

;; keep as polygons not segments
;; find first clockwise point, and then compare endpoint of segment in the same direction with next point.
;; If next clockwise point is before endpoint, check closer/further and change current polygon if need be
;; otherwise, next point clockwise is endpoint so presumably visible?
;; maybe need to assume clockwise polygons, counter clockwise points?
(defn visibility-tree [position polygon-edges]
  (loop [polygon-edges polygon-edges points []]
    (if (empty? polygon-edges)
      points
      (let [[p q _] (first polygon-edges)
            orient (tm/signum (v/orient2d position p q) 0.01)]
        (cond (neg? orient)
              (recur (rest polygon-edges) points)
              :else
              (recur (rest polygon-edges) (conj points p)))))))

;; FIXME: it's not clipping visible segments of a wall correctly
;; particularly on the bounding box.
;; references:
;; https://www.redblobgames.com/articles/visibility/
;; https://web.archive.org/web/20211002142937/https://sszczep.github.io/ray-casting-in-2d-game-engines/
;; http://www.dgp.toronto.edu/~ghali/publications/thesis/html/node9.html
(defn visible-regions [position polygons]
  (let [polygon-edges (->> (for [poly polygons
                                 [p q] (g/edges poly)]
                             [p q poly])
                           (sort-by (fn [[p _ _]] (- eq/TAU (g/heading (tm/- p position))))))
        points (visibility-tree position polygon-edges)]
    (->> (cons (last points) points)
         (partition 2 1)
         (map (fn [[p q]] (gt/triangle2 position p q))))))

(def scenes {:shapes gen-shapes
             :boxes gen-boxes})

(defn draw-state [{:keys [theta mouse]}]
  (q/ellipse-mode :radius)
  (q/background 1.0)
  (q/stroke 0.0)
  (q/stroke-weight 0.75)
  (q/no-fill)
  (reset! defo {})
  (let [{:keys [mode scene omnidirectional bounding-rectangle visible-shapes] :as ui-mode} @ui-state
        shapes (let [s ((get scenes scene) theta)]
                 (if bounding-rectangle
                   (conj s (cq/screen-rect 1.1))
                   s))
        segments (mapcat g/edges shapes)]
    (case mode
      :closest
      (doseq [angle (sm/range-subdivided tm/TWO_PI 200)]
        (let [ray [mouse (v/+polar mouse (q/width) angle)]]
          (when-let [intersection (closest-intersection ray segments)]
            (q/line mouse intersection))))
      :ray-march
      (let [world (world-map segments)]
        (if omnidirectional
          (doseq [angle (sm/range-subdivided tm/TWO_PI 45)
                  :let [[hit path] (ray-march mouse angle world)]]
            (draw-ray mouse hit path ui-mode))
          (let [angle (* theta 0.5)
                [hit path] (ray-march mouse angle world)]
            (draw-ray mouse hit path ui-mode))))
      :reflect-ray-march
      (let [world (world-map segments)
            angle (* theta 0.5)
            [_ path] (reflect-ray-march (* 4 (q/width)) 100 mouse angle world)]
        (when (:show-path ui-mode)
          (q/stroke-weight 0.4)
          (q/stroke 0.0 0.5 0.5)
          (doseq [[c r] path]
            (cq/circle c r)))
        (q/stroke-weight 0.8)
        (q/stroke 0.5)
        (cq/draw-path (mapv first path)))
      :visible-polygon
      (let [vertices (for [angle (sm/range-subdivided tm/TWO_PI 120)
                           :let [ray [mouse (v/+polar mouse (q/width) angle)]
                                 hit (closest-intersection ray segments)]
                           :when hit]
                       hit)]
        (q/background 0.8)
        (q/stroke-weight 0.8)
        (q/fill 1.0)
        (cq/draw-shape vertices)
        (q/no-stroke)
        (q/fill 0.155 0.6 0.6 1.0)
        (cq/circle mouse 3.0))
      :visible-regions
      (let [regions (visible-regions mouse shapes)]
        (q/background 0.8)
        (q/stroke 0.6)
        (q/stroke-weight 0.8)
        (q/fill 1.0)
        (doseq [region regions]
          (cq/draw-polygon region))
        (q/no-stroke)
        (q/fill 0.155 0.6 0.6 1.0)
        (cq/circle mouse 3.0)))

    (when visible-shapes
      (q/stroke 0.0)
      (q/stroke-weight 0.75)
      (q/no-fill)
      (draw-shapes shapes))))

(defn explanation []
  [:div
   [:p "In " [:em "closest"] " mode, rays are drawn from the selected origin to
   the closest segment along the path of the ray."]
   [:p "In " [:em "ray-march"] " mode, rays are drawn from the selected origin, but moving forward each step only as far as the closest segment to that point. The closest jump distance can be visualized if " [:em "closest surface radius"] " is enabled."]
   [:p "In " [:em "reflect-ray-march"] " mode, the ray will reflect on any segment hit instead of terminating immediately."]
   [:p "In " [:em "visible-polygon"] " mode, draws the polygon re-constructed from all the closest segment hits from a omnidirectional sweep."]
   [:p "Click inside the canvas to place the ray origin."]])

(def modes [:ray-march :reflect-ray-march :closest
            :visible-polygon :visible-regions])

(defn ui-controls []
  [:div.flexcols
   (ctrl/container
    (ctrl/checkbox ui-state "Animated" [:animated])
    (ctrl/change-mode ui-state modes)
    (ctrl/change-mode ui-state (keys scenes)
                      {:button-value "Cycle Scene"
                       :mode-desc "Scene: "
                       :mode-key :scene})
    (ctrl/checkbox ui-state "Include Bounding Rectangle in Shapes" [:bounding-rectangle])
    (ctrl/checkbox ui-state "Show Shapes" [:visible-shapes])
    (let [{:keys [mode]} @ui-state]
      (when (#{:ray-march :reflect-ray-march} mode)
        [:div
         (when (= :ray-march mode)
           (ctrl/checkbox ui-state "Omnidirectional" [:omnidirectional]))
         (ctrl/checkbox ui-state "Closest Surface Radius" [:show-path])]))
    [explanation])
   (debug/display defo)])

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw-state
    :middleware [m/fun-mode framerate/mode])
   [:div.contained.explanation
    [ui-controls]]])

(sketch/definition ray-marching
  {:created-at "2020-08-24"
   :tags #{}
   :type :quil}
  (ctrl/mount page "sketch-host"))
