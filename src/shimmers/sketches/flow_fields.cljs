(ns shimmers.sketches.flow-fields
  "https://tylerxhobbs.com/essays/2020/flow-fields"
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.string :as scs]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.color :as color]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.hexagon :as hex]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; TODO: play with some of the modes in
;; https://sighack.com/post/getting-creative-with-perlin-noise-fields
;; https://sighack.com/post/procedural-color-algorithms-value-keys

(def flows-per-iter 100)
(defonce settings
  (ctrl/state {:calc-points "flow-points"
               :draw "curves"
               :align-triangles true
               :point-source "random"
               :grid-divisor 8
               :snap-resolution "0"
               :region-specific-snap false
               :palette-mode "monochrome"
               :palette-color "#000000"
               :iterations 90
               :step-size 4
               :step-size-variance 0
               :stroke-weight 8
               :stroke-weight-variance 0
               :length 32
               :length-variance 0
               :noise-div 6
               :jitter 0
               :obstacles {:n 0 :points [] :radius 12
                           :display true
                           :voronoi false}}))

(defn variance [mu sd]
  (if (> sd 0)
    (int (abs (dr/gaussian mu sd)))
    mu))

(defn dir-at
  [[x y] noise-div]
  (* tm/TWO_PI (q/noise (/ x noise-div) (/ y noise-div))))

(defn snap-to [{:keys [snap-resolution region-specific-snap]} point theta]
  (let [resolution (if region-specific-snap
                     (let [[x y] point
                           f 0.0005
                           n (q/noise (* f x) (* f y))]
                       (int (Math/floor (* 12 (eq/sqr (- (* 2 n) 1))))))
                     snap-resolution)]
    (if (> resolution 0)
      (* (Math/round (/ theta resolution)) resolution)
      theta)))

(defn avoid-obstacles [p {:keys [points radius voronoi]}]
  (if-let [closest (apply min-key #(g/dist-squared p %) points)]
    ((if voronoi tm/* tm/normalize)
     (tm/- p closest) (/ (* radius radius) (g/dist-squared p closest)))
    (gv/vec2)))

(defn noise-point
  [{:keys [step-size step-size-variance
           noise-div jitter obstacles] :as state}
   point]
  (let [dir (snap-to state point (dir-at point noise-div))
        next-point (v/+polar point (variance step-size step-size-variance) dir)]
    (tm/+ next-point
          (avoid-obstacles next-point obstacles)
          (v/jitter (tm/random jitter)))))

(defn draw-grid [{:keys [length step-size noise-div jitter] :as state}]
  (let [w (/ (q/width) length)
        h (/ (q/height) length)]
    (doseq [[p dir]
            (for [x (range (* -2 length) (* (+ 3 w) length) length)
                  y (range (* -2 length) (* (+ 3 h) length) length)]
              [(gv/vec2 x y) (dir-at [x y] noise-div)])]
      (q/line p
              (-> p
                  (v/+polar step-size (snap-to state p dir))
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
  [p {:keys [length length-variance] :as settings}]
  (reductions (partial noise-point settings) p
              (range (variance length length-variance))))

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
  [p {:keys [step-size step-size-variance
             length length-variance
             noise-div snap-resolution jitter]}]
  (reductions
   (fn [p]
     (if-let [next-point (downhill p
                                   (variance step-size step-size-variance)
                                   noise-div snap-resolution)]
       (tm/+ p next-point (v/jitter (tm/random jitter)))
       (reduced p)))
   p (range (variance length length-variance))))

(defn points
  [{:keys [calc-points point-source] :as settings}]
  (calc-points (point-source) settings))

(defn draw-triangles [triangle {:keys [align-triangles] :as settings}]
  (let [points (points settings)]
    (doseq [[p q] (partition 2 1 points)
            :let [theta (if align-triangles
                          (g/heading (tm/- q p))
                          (dr/random-tau))]]
      (apply cq/draw-triangle
             (-> triangle
                 (g/rotate theta)
                 (g/center p)
                 :points)))))

(defn palettes [_ color-str]
  (let [colors (map color/hex->hsla (str/split color-str #","))]
    (dr/rand-nth colors)))

(defn point-generator [source grid-divisor]
  (case source
    "random" (fn [] (cq/rel-vec (dr/random) (dr/random)))
    "center" (let [c (gc/circle (cq/rel-vec 0.5 0.5) (cq/rel-h 0.35))]
               (partial rp/inside-circle c))
    "grid" (let [{[w h] :size :as rect} (cq/screen-rect 1.05)
                 grid (time (->> {:cols (int (/ w grid-divisor))
                                  :rows (int (/ h grid-divisor))}
                                 (g/subdivide rect)
                                 (mapv g/centroid)))]
             (rp/sample-pool-replacement grid))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/background 1.0)
  (q/noise-seed (dr/random 1000000))
  (let [{:keys [iterations draw align-triangles
                calc-points point-source grid-divisor
                palette-mode palette-color
                snap-resolution region-specific-snap
                stroke-weight stroke-weight-variance
                length length-variance
                step-size step-size-variance
                noise-div jitter obstacles]}
        @settings]
    {:iter 0
     :iterations iterations
     :calc-points (get {"flow-points" flow-points
                        "downhill-points" downhill-points}
                       calc-points)
     :point-source (point-generator point-source grid-divisor)
     :grid-divisor grid-divisor
     :snap-resolution (edn/read-string snap-resolution)
     :region-specific-snap region-specific-snap
     :palette-mode palette-mode
     :palette-color palette-color
     :step-size step-size
     :step-size-variance (* step-size (/ step-size-variance 100))
     :stroke-weight (/ 1 stroke-weight)
     :stroke-weight-variance (* (/ 1 stroke-weight) (/ stroke-weight-variance 100))
     :noise-div (Math/pow 2 noise-div)
     :draw draw
     :align-triangles align-triangles
     :length length
     :length-variance (* length (/ length-variance 100))
     :jitter (* step-size (if (> jitter 0) (/ 1 jitter) 0))
     :obstacles (assoc obstacles :points
                       (repeatedly (:n obstacles) #(cq/rel-vec (dr/random-vertex))))}))

(defn update-state [state]
  (update state :iter inc))

(defn draw
  [{:keys [palette-mode palette-color
           stroke-weight stroke-weight-variance
           step-size iter iterations
           draw obstacles]
    :as settings}]
  (q/stroke-weight (* 4 stroke-weight))
  (q/stroke 0.0 0.0 0.0 1.0)
  (q/ellipse-mode :radius)
  (q/fill 1.0)
  (when (:display obstacles)
    (doseq [p (:points obstacles)]
      (cq/circle p (/ (:radius obstacles) 4))))
  (q/no-fill)
  (q/stroke-weight (abs (dr/gaussian stroke-weight stroke-weight-variance)))
  (when (< iter iterations)
    (apply q/stroke (palettes palette-mode palette-color))
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
   :grid-divisor [4 32]
   :region-specific-snap #{true false}
   :snap-resolution
   {"Disabled" 0
    "90 degrees" (/ Math/PI 2)
    "60 degrees" (/ Math/PI 3)
    "45 degrees" (/ Math/PI 4)
    "30 degrees" (/ Math/PI 6)
    "20 degrees" (/ Math/PI 9)
    "15 degrees" (/ Math/PI 12)
    "10 degrees" (/ Math/PI 18)}
   :palette-mode
   {"Monochrome" "monochrome"
    "Random Saturation/Lightness" "random-sl"}
   :palette-color {"black" "#000000"
                   "red" "#ff0000"
                   "blue" "#2222ff"}
   :iterations [1 500]
   :stroke-weight [1 64]
   :stroke-weight-variance [0 200]
   :step-size [1 64]
   :step-size-variance [0 200]
   :length [8 128]
   :length-variance [0 200]
   :noise-div [0 12 0.1]
   :jitter [0 32]})

;; TODO: could controls auto-populate ranges into a ui-mappings object?
;; That would help with shuffling on other sketches and could assist in encoding settings in the URL?
(defn shuffle-settings []
  (-> (into {}
            (for [[k v] ui-mappings]
              (cond (map? v) [k (dr/rand-nth (vals v))]
                    (set? v) [k (dr/rand-nth (vec v))]
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
    (when (= (:point-source @settings) "grid")
      (ctrl/slider settings (partial str "Grid Divisor ") [:grid-divisor] (:grid-divisor ui-mappings)))
    (when-not (= (:calc-points @settings) "downhill-points")
      (ctrl/checkbox settings "Region Specific Snap" [:region-specific-snap]))
    (when-not (:region-specific-snap @settings)
      (ctrl/dropdown settings "Snap Angles To "
                     [:snap-resolution] (:snap-resolution ui-mappings)))
    #_(ctrl/dropdown settings "Palette Mode" [:palette-mode] (:palette-mode ui-mappings))
    (ctrl/palette-colors settings "Palette Color(s)" [:palette-color])
    (ctrl/slider settings (fn [v] (str "Iterations " (* flows-per-iter v)))
                 [:iterations] (:iterations ui-mappings))
    (ctrl/slider settings (fn [v] (scs/format "Stroke Weight %7.4f" (/ 1 v)))
                 [:stroke-weight] (:stroke-weight ui-mappings))
    (ctrl/numeric settings "Stroke Weight Variance"
                  [:stroke-weight-variance] (:stroke-weight-variance ui-mappings))
    (ctrl/slider settings (fn [v] (str "Step Size " v))
                 [:step-size] (:step-size ui-mappings))
    (ctrl/numeric settings "Step Size Variance"
                  [:step-size-variance] (:step-size-variance ui-mappings))
    (ctrl/slider settings (fn [v] (str "Length " v)) [:length] (:length ui-mappings))
    (ctrl/numeric settings "Length Variance"
                  [:length-variance] (:length-variance ui-mappings))
    (ctrl/slider settings (fn [v] (scs/format "Noise Multiplier 1/%.1f" (Math/pow 2 v)))
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
                     (view-sketch/restart-sketch :flow-fields))}
     "Shuffle Settings"]]])

(defn page []
  [:div
   (sketch/component
    :size [1024 768]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:div.contained.explanation
    [ui-controls]]])

(sketch/definition flow-fields
  {:created-at "2021-06-17"
   :tags #{:static :deterministic}
   :type :quil}
  (ctrl/mount page "sketch-host"))
