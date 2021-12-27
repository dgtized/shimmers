(ns shimmers.sketches.flow-fields
  "https://tylerxhobbs.com/essays/2020/flow-fields"
  (:require [clojure.edn :as edn]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.deterministic-random :as dr]
            [shimmers.math.equations :as eq]
            [shimmers.math.geometry :as geometry]
            [shimmers.math.hexagon :as hex]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]
            [shimmers.view.sketch :as view-sketch]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]
            [thi.ng.strf.core :as f]))

;; TODO: play with some of the modes in
;; https://sighack.com/post/getting-creative-with-perlin-noise-fields
;; https://sighack.com/post/procedural-color-algorithms-value-keys

(def flows-per-iter 100)
(defonce settings
  (ctrl/state {:calc-points "flow-points"
               :draw "curves"
               :align-triangles true
               :point-source "random"
               :snap-resolution "0"
               :iterations 90
               :step-size 4
               :stroke-weight 8
               :length 32
               :noise-div 6
               :jitter 0
               :obstacles {:n 0 :points [] :radius 12
                           :display true
                           :voronoi false}}))

(defn dir-at
  [[x y] noise-div]
  (* tm/TWO_PI (q/noise (/ x noise-div) (/ y noise-div))))

(defn snap-to [theta resolution]
  (if (> resolution 0)
    (* (Math/round (/ theta resolution)) resolution)
    theta))

(defn avoid-obstacles [p {:keys [points radius voronoi]}]
  (if-let [closest (apply min-key #(g/dist-squared p %) points)]
    ((if voronoi tm/* tm/normalize)
     (tm/- p closest) (/ (* radius radius) (g/dist-squared p closest)))
    (gv/vec2)))

(defn noise-point
  [{:keys [step-size noise-div snap-resolution jitter obstacles]}
   point]
  (let [dir (snap-to (dir-at point noise-div) snap-resolution)
        next-point (tm/+ point (v/polar step-size dir))]
    (tm/+ next-point
          (avoid-obstacles next-point obstacles)
          (v/jitter (tm/random jitter)))))

(defn draw-grid [{:keys [length step-size noise-div snap-resolution jitter]}]
  (let [w (/ (q/width) length)
        h (/ (q/height) length)]
    (doseq [[p dir]
            (for [x (range (* -2 length) (* (+ 3 w) length) length)
                  y (range (* -2 length) (* (+ 3 h) length) length)]
              [(gv/vec2 x y) (dir-at [x y] noise-div)])]
      (q/line p
              (-> p
                  (v/add (v/polar step-size (snap-to dir snap-resolution)))
                  (v/add (v/jitter (tm/random jitter))))))))

(defn pointy-hexagon [r [x y]]
  (for [i (range 0 6)]
    (let [angle (+ (* i (/ Math/PI 3)) (/ Math/PI 6))
          hx (+ x (* r (Math/cos angle)))
          hy (+ y (* r (Math/sin angle)))]
      (gv/vec2 hx hy))))

;; Inspired by https://www.bit-101.com/blog/2019/01/perlinized-hexagons/
(defn draw-hexagon-grid [{:keys [length] :as settings}]
  (let [width (int (/ (q/width) length))
        height (int (/ (q/height) length))]
    (doseq [hex (for [gx (range width)
                      gy (range height)]
                  (->> [gx gy]
                       hex/oddr->cube
                       (hex/cube-pointy->pixel length)
                       (pointy-hexagon length)
                       (map (partial noise-point settings))))]
      (cq/draw-shape hex))))

(defn flow-points
  [p {:keys [length] :as settings}]
  (reductions (partial noise-point settings) p (range length)))

(defn angles [r resolution]
  (map (fn [theta] (v/polar r theta))
       (range 0 tm/TWO_PI resolution)))

(comment (angles 1 (/ Math/PI 6)))

(defn downhill [[x y] r noise-div snap-resolution]
  (let [surroundings
        (for [[dx dy] (angles r (if (> snap-resolution 0)
                                  snap-resolution
                                  (/ tm/TWO_PI 60)))]
          [[dx dy]
           (q/noise (/ (+ x dx) noise-div)
                    (/ (+ y dy) noise-div))])
        [[px py] minimum] (apply min-key second surroundings)]
    (when (> (q/noise (/ x noise-div) (/ y noise-div)) minimum)
      (gv/vec2 px py))))

(defn downhill-points
  [p {:keys [step-size length noise-div snap-resolution jitter]}]
  (reductions
   (fn [p]
     (if-let [next-point (downhill p step-size noise-div snap-resolution)]
       (tm/+ p next-point (v/jitter (tm/random jitter)))
       (reduced p)))
   p (range length)))

(defn points
  [{:keys [calc-points point-source] :as settings}]
  (calc-points (point-source) settings))

(defn draw-triangles [triangle {:keys [align-triangles] :as settings}]
  (let [points (points settings)]
    (doseq [[p q] (partition 2 1 points)
            :let [theta (if align-triangles
                          (g/heading (tm/- q p))
                          (dr/random eq/TAU))]]
      (apply cq/draw-triangle
             (-> triangle
                 (g/rotate theta)
                 (g/center p)
                 :points)))))

(defn point-generator [source]
  (case source
    "random" (fn [] (cq/rel-vec (dr/random) (dr/random)))
    "center" (let [c (gc/circle (cq/rel-vec 0.5 0.5) (cq/rel-h 0.35))]
               (fn [] (geometry/random-point-in-circle c)))
    "grid" (let [{[w h] :size :as rect} (cq/screen-rect 1.05)
                 grid (time (g/subdivide rect {:cols (* 0.5 w) :rows (* 0.5 h)}))
                 points (atom (dr/shuffle (mapv g/centroid grid)))]
             (println (count grid))
             (fn [] (let [[v & r] @points]
                     (reset! points (if (seq r) r (dr/shuffle grid)))
                     v)))))

(defn validate! [settings]
  (let [{:keys [iterations point-source]} @settings]
    (println iterations)
    (swap! settings assoc :iterations
           (if (= point-source "grid")
             (min iterations 30)
             iterations))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/background 1.0)
  (q/noise-seed (dr/random 1000000))
  (validate! settings)
  (let [{:keys [iterations draw align-triangles
                calc-points point-source
                snap-resolution stroke-weight
                length step-size noise-div jitter obstacles]}
        @settings]
    {:iter 0
     :iterations iterations
     :calc-points (get {"flow-points" flow-points
                        "downhill-points" downhill-points}
                       calc-points)
     :point-source (point-generator point-source)
     :snap-resolution (edn/read-string snap-resolution)
     :step-size step-size
     :stroke-weight (/ 1 stroke-weight)
     :noise-div (Math/pow 2 noise-div)
     :draw draw
     :align-triangles align-triangles
     :length length
     :jitter (* step-size (if (> jitter 0) (/ 1 jitter) 0))
     :obstacles (assoc obstacles :points
                       (repeatedly (:n obstacles) #(cq/rel-vec (dr/random-vertex))))}))

(defn update-state [state]
  (update state :iter inc))

(defn draw
  [{:keys [stroke-weight step-size iter iterations draw obstacles]
    :as settings}]
  (q/stroke-weight (* 4 stroke-weight))
  (q/stroke 0.0 0.0 0.0 1.0)
  (q/ellipse-mode :radius)
  (q/fill 1.0)
  (when (:display obstacles)
    (doseq [p (:points obstacles)]
      (cq/circle p (/ (:radius obstacles) 4))))
  (q/no-fill)
  (q/stroke-weight stroke-weight)
  (when (< iter iterations)
    (let [hstep (* step-size 0.5)]
      (case draw
        "segments"
        (dotimes [_ flows-per-iter]
          (cq/draw-path (points settings)))
        "curves"
        (dotimes [_ flows-per-iter]
          (cq/draw-curve-path (points settings)))
        "grid"
        (draw-grid settings)
        "hexagons"
        (draw-hexagon-grid settings)
        "circles"
        ;; alternative, do circle packing, no-overlap?
        (dotimes [_ (/ flows-per-iter 4)]
          (doseq [p (points settings)]
            (cq/circle p hstep)))
        "triangles"
        (let [triangle (gt/triangle2 [hstep 0] [(- hstep) hstep] [(- hstep) (- hstep)])]
          (dotimes [_ (/ flows-per-iter 4)]
            (draw-triangles triangle settings)))))))

;; TODO handle align-triangles and obstacles
(def ui-mappings
  {:calc-points
   {"Angle from Noise" "flow-points"
    "Flow Downhill" "downhill-points"}
   :draw
   {"Curved Lines" "curves"
    "Segmented Lines" "segments"
    "Circles" "circles"
    "Triangles" "triangles"
    "Hexagon Grid" "hexagons"
    "Debug Grid" "grid"}
   :point-source
   {"Random" "random"
    "Center" "center"
    "Grid" "grid"}
   :snap-resolution
   {"Disabled" 0
    "90 degrees" (/ Math/PI 2)
    "60 degrees" (/ Math/PI 3)
    "45 degrees" (/ Math/PI 4)
    "30 degrees" (/ Math/PI 6)
    "20 degrees" (/ Math/PI 9)
    "15 degrees" (/ Math/PI 12)
    "10 degrees" (/ Math/PI 18)}
   :iterations [1 500]
   :stroke-weight [1 64]
   :step-size [1 64]
   :length [8 128]
   :noise-div [0 12 0.1]
   :jitter [0 32]})

;; TODO: could controls auto-populate ranges into a ui-mappings object?
;; That would help with shuffling on other sketches and could assist in encoding settings in the URL?
(defn shuffle-settings []
  (-> (into {}
            (for [[k v] ui-mappings]
              (cond (map? v) [k (dr/rand-nth (vals v))]
                    (vector? v) [k (dr/rand-nth (apply range v))])))
      (update-in [:snap-resolution] str)))

(defn ui-controls []
  [:div.flexcols
   (ctrl/container
    (ctrl/dropdown settings "Algorithm" [:calc-points] (:calc-points ui-mappings))
    (ctrl/dropdown settings "Draw" [:draw] (:draw ui-mappings))
    (when (= (:draw @settings) "triangles")
      (ctrl/checkbox settings "Align Triangles" [:align-triangles]))
    (ctrl/dropdown settings "Point Source" [:point-source] (:point-source ui-mappings))
    (ctrl/dropdown settings "Snap Angles To "
                   [:snap-resolution] (:snap-resolution ui-mappings))
    (ctrl/slider settings (fn [v] (str "Iterations " (* flows-per-iter v)))
                 [:iterations] (:iterations ui-mappings))
    (ctrl/slider settings (fn [v] (str "Stroke Weight " (/ 1 v)))
                 [:stroke-weight] (:stroke-weight ui-mappings))
    (ctrl/slider settings (fn [v] (str "Step Size " v))
                 [:step-size] (:step-size ui-mappings))
    (ctrl/slider settings (fn [v] (str "Length " v)) [:length] (:length ui-mappings))
    (ctrl/slider settings (fn [v] (f/format ["Noise Multiplier 1/" (f/float 1)] (Math/pow 2 v)))
                 [:noise-div] (:noise-div ui-mappings))
    (ctrl/slider settings (fn [v] (if (> v 0) (str "Jitter 1/" v " * step-size") "No Jitter"))
                 [:jitter] (:jitter ui-mappings))
    (when (= (:calc-points @settings) "flow-points")
      [:div
       (ctrl/slider settings (fn [v] (if (pos? v) (str "Obstacles " v)
                                        "No Obstacles"))  [:obstacles :n] [0 64])
       (when (pos? (get-in @settings [:obstacles :n]))
         [:div.indent
          (ctrl/slider settings (fn [v] (str "Radius " v)) [:obstacles :radius] [2 128])
          (ctrl/checkbox-after settings "Display" [:obstacles :display])
          (ctrl/checkbox-after settings "Voronoi" [:obstacles :voronoi])])]))
   [:div
    (view-sketch/generate :flow-fields)
    [:button.generate
     {:style {:margin-left "1em"}
      :on-click #(do (swap! settings merge (shuffle-settings))
                     (view-sketch/restart-sketch {:id :flow-fields}))}
     "Shuffle Settings"]]])

(sketch/defquil flow-fields
  :created-at "2021-06-17"
  :tags #{:static :deterministic}
  :on-mount (fn [] (ctrl/mount ui-controls))
  :size [1200 900]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
