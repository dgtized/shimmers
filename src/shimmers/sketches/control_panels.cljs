(ns shimmers.sketches.control-panels
  (:require
   [clojure.math :as math]
   [shimmers.algorithm.square-packing :as square]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(def screen (rect/rect 0 0 width height))
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn relative-polar-vector
  "Translate from a point `p` with distance `radius` along an angle `theta`"
  [p radius theta]
  (tm/+ p (g/as-cartesian (gv/vec2 radius theta))))

;; Alternative short name might be +pv, as in plus polar vector.
(def rpv relative-polar-vector)

(defn slider [bounds vertical percent]
  (let [slider-box (g/scale-size bounds 0.9)
        [low high] (if vertical
                     [(gv/vec2 0.15 0) (gv/vec2 0.85 1)]
                     [(gv/vec2 0 0.15) (gv/vec2 1 0.85)])
        slider (rect/rect (g/unmap-point slider-box low)
                          (g/unmap-point slider-box high))
        inner (g/scale-size slider 0.95)
        spacer 0.02]
    (csvg/group {}
      slider
      (for [t (range 0 1 0.1)]
        (gl/line2 (g/unmap-point slider (if vertical (gv/vec2 0.0 t) (gv/vec2 t 1.0)))
                  (g/unmap-point slider (if vertical (gv/vec2 0.1 t) (gv/vec2 t 0.9)))))
      (with-meta
        (rect/rect (g/unmap-point inner (if vertical
                                          (gv/vec2 0.1 (- percent spacer))
                                          (gv/vec2 (- percent spacer) 0.1)))
                   (g/unmap-point inner (if vertical
                                          (gv/vec2 0.9 (+ percent spacer))
                                          (gv/vec2 (+ percent spacer) 0.9))))
        {:rx 3}))))

(defn order-on [axis]
  (fn [{:keys [p]}] (axis p)))

(defn level-meter [bounds levels]
  (let [[tier0 tier1 coord dir scale]
        (if (> (g/width bounds) (g/height bounds))
          (let [s 10]
            [{:cols (count levels) :rows 1} {:cols 1 :rows s} (order-on :y) > (/ 10 s)])
          (let [s 10]
            [{:cols 1 :rows (count levels)} {:cols s :rows 1} (order-on :x) < (/ 10 s)]))
        level-sets (g/subdivide (geometry/inset-rectangle bounds 4) tier0)]
    (csvg/group {}
      (mapcat (fn [level indicator-line]
                (for [[i cell] (->> tier1
                                    (g/subdivide (g/scale-size indicator-line 0.9))
                                    (sort-by coord dir)
                                    (map-indexed vector))
                      :let [cell (geometry/inset-rectangle cell 3)]]
                  (if (<= (* i scale) level)
                    (csvg/group {}
                      [cell (geometry/inset-rectangle cell 3)])
                    cell)))
              levels level-sets))))

(defn vu-meter [center r pct]
  (let [p (tm/+ center (gv/vec2 0 (* 0.66 r)))
        t0 (* (/ 7 6) math/PI)
        t1 (* (/ 11 6) math/PI)
        inner (* 0.75 r)
        upper (* 0.9 r)
        lower (* 0.8 r)
        theta (tm/mix* t0 t1 pct)]
    (csvg/group {}
      (csvg/path [[:M (rpv p r t0)]
                  [:A [r r] 0 0 1 (rpv p r t1)]
                  [:L (rpv p inner t1)]
                  [:A [inner inner] 0 0 0 (rpv p inner t0)]
                  [:Z]])
      (for [t (map #(+ % 0.05) (range t0 t1 0.1))]
        (gl/line2 (rpv p lower t) (rpv p upper t)))
      (gl/line2 (rpv p (* 0.5 r) theta)
                (rpv p (* 0.95 r) theta))
      ;; bounding box
      (with-meta (rect/rect (tm/+ center (gv/vec2 (- r) (* -0.5 r)))
                            (tm/+ center (gv/vec2 (+ r) (* 0.5 r))))
        {:rx 10}))))

(defn oscilliscope [center r]
  (let [rect (g/center (rect/rect (* 2 r)) center)]
    (csvg/group {}
      (with-meta rect
        {:rx 25})
      (for [t (range 0.1 0.9 0.2)]
        (gl/line2 (g/unmap-point rect (gv/vec2 t 0.025))
                  (g/unmap-point rect (gv/vec2 t 0.975))))
      (for [t (range 0.1 0.9 0.2)]
        (gl/line2 (g/unmap-point rect (gv/vec2 0.025 t))
                  (g/unmap-point rect (gv/vec2 0.975 t)))))))

(defn indicator-light [p r on]
  (csvg/group {}
    (gc/circle p (* 0.6 r))
    (with-meta (gc/circle p (* 0.4 r))
      {:fill (if on "black" "white")})))

(defn plug [p r label]
  (csvg/group {}
    (when label
      (-> (rect/rect 0 0 (* 2.3 r) (* 2.6 r))
          (g/center)
          (g/translate (tm/+ p (gv/vec2 0 (* 0.18 r))))
          (with-meta {:rx 2})))
    (gc/circle p (* 0.65 r))
    (gc/circle p (* 0.95 r))))

(defn rect-button
  ([p r rx] (rect-button p r r rx))
  ([p w h rx]
   (->
    (rect/rect 0 0 w h)
    g/center
    (g/translate p)
    (with-meta {:rx rx}))))

(defn button [p r style label on]
  (csvg/group {}
    (conj (case style
            :circle [(gc/circle p (* 1.15 r))
                     (gc/circle p (* 1 r))]
            :square [(rect-button p (* 2 r) 5)
                     (rect-button p (* 1.75 r) 3)]
            :indicator-toggle
            [(rect-button (tm/+ p (gv/vec2 0 (* 0.25 r)))
                          (* 1.4 r) (* 1.75 r) 3)
             (rect-button (tm/+ p (gv/vec2 0 (* 0.66 r)))
                          (* 1.33 r) (* 0.95 r) 3)
             (indicator-light (tm/- p (gv/vec2 0 (* 0.2 r))) (* 0.3 r) on)])
          (let [text-size (int (tm/map-interval-clamped (max (- r 12) 0) [0 30] [8 16]))]
            (csvg/center-label (if (= style :indicator-toggle)
                                 (tm/- p 0 (* 0.9 r))
                                 p)
                               label
                               {:style {:font (str "bold " text-size "px sans-serif")}})))))

(defn ridged [p r ridges theta]
  (let [d (* 0.03 r)
        w (/ math/PI (* 2 ridges))]
    (gp/polygon2 (sequence (mapcat (fn [v]
                                     (let [t (+ (* eq/TAU (/ v ridges)) theta)]
                                       (map (fn [[dr dt]]
                                              (v/+polar p (+ (* 0.8 r) dr) (+ t dt)))
                                            [[(- d) (- w)]
                                             [0 0]
                                             [d w]]))))
                           (range ridges)))))

(defn indicator-line [{:keys [p r r0 r1 width theta]}]
  (with-meta (-> (rect/rect (gv/vec2 (* r0 r) (* (- width) r))
                            (gv/vec2 (* r1 r) (* width r)))
                 (g/rotate theta)
                 (g/translate p))
    {:rx 10 :fill "white" :stroke-width 0.66}))

(defn radial-tick-lines [p r mapper ticks]
  (for [t (range 0 1 (/ 1 ticks))]
    (gl/line2 (rpv p (* 0.90 r) (mapper t))
              (rpv p (* 1.00 r) (mapper t)))))

(defn knob [p r pct
            {:keys [surface ticks dedants]
             :or {surface :smooth
                  ticks 10
                  dedants [(* 0.5 eq/TAU) (* 1.25 eq/TAU)]}}]
  (let [mapper (fn [t] (tm/map-interval t [0 1] dedants))
        theta (mapper pct)]
    (csvg/group {}
      (concat
       (case surface
         :smooth [(gc/circle p (* 0.9 r))]
         :ridged (let [ridged (ridged p r 15 theta)]
                   [ridged
                    (g/scale-size ridged 0.85)])
         :hex [(gc/circle p (* 0.9 r))
               (-> (gc/circle (* 0.75 r))
                   (g/as-polygon 6)
                   (g/rotate (+ theta (/ eq/TAU 12)))
                   (g/translate p))])
       [(radial-tick-lines p r mapper ticks)
        (indicator-line
         (merge {:p p :r r :theta theta}
                (case surface
                  :smooth
                  {:r0 0.4 :r1 1.025 :width 0.08}
                  :ridged
                  {:r0 0.3 :r1 0.85 :width 0.1}
                  :hex
                  {:r0 0.2 :r1 0.7 :width 0.1})))]))))

(defn toggle-switch [p r vertical dip on]
  (let [dir (gv/vec2 0 (if on -1 1))]
    (if dip
      (csvg/group {}
        (-> (if vertical
              (rect/rect 0 0 r (* 1.25 r))
              (rect/rect 0 0 (* 1.25 r) r))
            (g/center)
            (g/translate p)
            (with-meta {:rx 3}))
        (-> (if vertical
              (rect/rect 0 0 (* 0.90 r) (* 0.66 r))
              (rect/rect 0 0 (* 0.66 r) (* 0.90 r)))
            (g/center)
            (g/translate (tm/+ p (if vertical
                                   (gv/vec2 0 (if on (* -0.25 r) (* 0.25 r)))
                                   (gv/vec2 (if on (* -0.25 r) (* 0.25 r)) 0))))
            (with-meta {:rx 5})))
      (csvg/group {:fill "white"
                   :transform (csvg/transform (csvg/translate p)
                                              (csvg/rotate (if vertical 0 (/ math/PI 2))))}
        (-> (gc/circle (* r 0.55))
            (g/as-polygon 6)
            (g/rotate (/ eq/TAU 12)))
        (gc/circle (gv/vec2) (* 0.12 r))
        (gc/circle (tm/* (gv/vec2 0 (* 0.25 r)) dir) (* 0.175 r))))))

(defn divide-panels [{[w h] :size :as bounds}]
  (let [area-ratio (/ (g/area bounds) (g/area screen))
        p-done (cond (< area-ratio 0.1) 1
                     (> area-ratio 0.65) 0
                     :else (- 1.0 area-ratio))]
    (if (dr/chance p-done)
      [bounds]
      (let [div
            (fn [p q]
              (fn [b]
                [(gv/vec2 p) (gv/vec2 q)
                 (if (dr/chance 0.2)
                   (dr/rand-nth [:clockwise :counter-clockwise :all])
                   (square/row-major b))]))
            divisions {(div [0.3 0.0] [0.7 1.0]) (if (> w h) 1 0)
                       (div [0.4 0.0] [1.0 0.5]) 1
                       (div [0.0 0.0] [0.6 0.5]) 1
                       (div [0.0 0.0] [0.5 0.3]) 1
                       (div [0.2 0.0] [0.8 0.8]) (if (> w h) 1 0)
                       (div [0.2 0.2] [0.8 1.0]) (if (> w h) 1 0)}
            region ((dr/weighted divisions) bounds)]
        (mapcat (fn [s] (divide-panels s))
                (apply square/punch-out-relative bounds region))))))

(defn subdivision-dims [{[w h] :size} size]
  {:rows (int (/ h size)) :cols (int (/ w size))})

(defn subdivision-count [bounds size]
  (let [{:keys [rows cols]} (subdivision-dims bounds size)]
    (* rows cols)))

(defn subdivide-to-cells [bounds size]
  (g/subdivide bounds (subdivision-dims bounds size)))

(defn random-label []
  (let [latin (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        lycian (map (fn [p] (js/String.fromCodePoint p)) (range 0x10280 0x1029D))
        coptic (map (fn [p] (js/String.fromCodePoint p)) (range 0x2C80 0x2CB1 2))
        runic (map (fn [p] (js/String.fromCodePoint p)) (range 0x16A0 0x16EA))
        bamum (map (fn [p] (js/String.fromCodePoint p)) (range 0xA6A0 0xA6EF))
        numbers (map str (range 0 10))
        pool (concat latin latin coptic runic lycian bamum numbers)]
    (apply str
           (repeatedly (dr/weighted {0 1
                                     1 2
                                     2 4
                                     3 2})
                       #(dr/rand-nth pool)))))

(defn assign-pane [{[w h] :size :as bounds}]
  (let [min-edge (min w h)
        area-ratio (/ (g/area bounds) (g/area screen))
        weights {:vertical-sliders (if (< h (* 0.12 height)) 0 0.8)
                 :horizontal-sliders (if (< w (* 0.12 width)) 0 0.8)
                 :indicator-light (cond (< min-edge 60) 3
                                        (< area-ratio 0.03) 2
                                        :else 0)
                 :vu-meter (cond (< min-edge (* 0.08 width))
                                 0
                                 (> area-ratio 0.2)
                                 1.5
                                 :else
                                 0.33)
                 :level-meter
                 (cond (> area-ratio 0.25)
                       0.66
                       (< area-ratio 0.05)
                       0.05
                       :else 0.5)
                 :knobs (cond (> area-ratio 0.20)
                              0.33
                              (> area-ratio 0.15)
                              0.66
                              :else 2)
                 :button (if (> area-ratio 0.2)
                           0.1
                           1.5)
                 :toggle-switch (if (> area-ratio 0.2)
                                  0.1
                                  1.5)
                 :plugs (cond (< area-ratio 0.02)
                              2
                              (< area-ratio 0.05)
                              1
                              (< area-ratio 0.1)
                              0.25
                              :else 0)
                 :oscilliscope (if (and (tm/delta= w h (* 0.33 min-edge))
                                        (> area-ratio 0.2)) 3.0 0.0)
                 :subdivide (cond (< area-ratio 0.06) 0.0
                                  (< area-ratio 0.1) 0.2
                                  (< area-ratio 0.2) 1
                                  (> area-ratio 0.3) 5
                                  :else 3)}]
    (case (dr/weighted weights)
      :subdivide
      (mapcat assign-pane
              (let [splits (dr/weighted {2 2
                                         3 1})]
                (if (> w h)
                  (g/subdivide bounds {:rows 1 :cols splits})
                  (g/subdivide bounds {:rows splits :cols 1}))))
      :vertical-sliders
      (let [n (dr/random-int 2 6)
            size (min (max (int (/ w n)) (* 0.05 width)) (* 0.12 width))]
        (for [s (g/subdivide bounds {:rows 1 :cols (int (/ w size))})]
          (slider s true (dr/random))))
      :horizontal-sliders
      (let [n (dr/random-int 1 4)
            size (min (max (int (/ h n)) (* 0.05 height)) (* 0.12 height))]
        (for [s (g/subdivide bounds {:rows (int (/ h size)) :cols 1})]
          (slider s false (dr/random))))
      :level-meter
      (let [n (dr/weighted {2 2 3 2 5 2 6 3 8 2 10 1})]
        (level-meter bounds (repeatedly n #(dr/random-int 10))))
      :knobs
      (let [size (max (* (dr/rand-nth [0.25 0.33 0.5]) min-edge)
                      (* 0.06 (min width height)))
            cnt (subdivision-count bounds size)
            size (min (cond (> cnt 40) (* size 4)
                            (> cnt 32) (* size 3)
                            (> cnt 16) (* size 2)
                            :else size)
                      min-edge)
            opts {:surface (dr/rand-nth [:smooth :ridged :hex])
                  :dedants (dr/weighted {[(* 0.5 eq/TAU) (* 1.25 eq/TAU)] 2
                                         [(* 0.25 eq/TAU) (* 1.0 eq/TAU)] 1
                                         [(* 0.4 eq/TAU) (* 1.1 eq/TAU)] 2
                                         [(* 0.35 eq/TAU) (* 1.15 eq/TAU)] 1
                                         [(* 0.3 eq/TAU) (* 1.2 eq/TAU)] 1})}]
        (for [s (subdivide-to-cells bounds size)]
          (knob (g/centroid s) (* 0.33 size) (dr/random) opts)))
      :toggle-switch
      (let [size (max (* (dr/rand-nth [0.25 0.33 0.5]) min-edge)
                      (* 0.06 (min width height)))
            cnt (subdivision-count bounds size)
            size (min (cond (> cnt 40) (* size 4)
                            (> cnt 32) (* size 3)
                            (> cnt 16) (* size 2)
                            :else size)
                      min-edge)
            vertical (dr/chance 0.5)
            dip (dr/chance 0.5)]
        (for [s (subdivide-to-cells bounds size)]
          (toggle-switch (g/centroid s) (* 0.5 size) vertical dip (dr/chance 0.5))))
      :vu-meter
      (let [n (dr/random-int 2 4)
            opts (cond (> w (* 1.8 h)) {:rows 1 :cols n}
                       (> h (* 2 w)) {:rows n :cols 1}
                       :else {:rows 1 :cols 1})]
        (for [{[w1 h1] :size :as s} (g/subdivide bounds opts)]
          (vu-meter (g/centroid s) (* 0.45 (min w1 h1)) (dr/random))))
      :oscilliscope
      (oscilliscope (g/centroid bounds) (* 0.45 min-edge))
      :plugs
      (let [size (* (dr/rand-nth [0.05 0.066 0.08]) (min width height))
            label (dr/chance 0.33)]
        (for [s (subdivide-to-cells bounds size)]
          (plug (g/centroid s) (* 0.3 size) label)))
      :button
      (let [scale (dr/rand-nth [0.08 0.12 0.16])
            size (* scale (min width height))
            style (dr/weighted {:square 1
                                :circle 1
                                :indicator-toggle (if (> scale 0.09) 1 0)})]
        (for [s (subdivide-to-cells bounds size)]
          (button (g/centroid s) (* 0.3 size) style (random-label) (dr/chance 0.5))))
      :indicator-light
      (let [size (* 0.05 (min width height))]
        (for [s (subdivide-to-cells bounds size)]
          (indicator-light (g/centroid s) (* 0.33 size) (dr/chance 0.5)))))))

(defn shapes []
  (let [bounds (g/scale-size screen 0.99)
        panels (mapv (fn [s] (with-meta (geometry/inset-rectangle s 3) {:rx 10}))
                     (divide-panels bounds))]
    (concat panels (mapcat assign-pane panels))))

(defn scene []
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.75}
    (shapes)))

(sketch/definition control-panels
  {:created-at "2022-02-07"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/static-page scene :control-panels)))
