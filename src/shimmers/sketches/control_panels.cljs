(ns shimmers.sketches.control-panels
  (:require
   [shimmers.algorithm.square-packing :as square]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(def screen (rect/rect 0 0 width height))
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn vertical-slider [rect pct]
  (let [slider0 (g/scale-size rect 0.9)
        slider (rect/rect (g/unmap-point slider0 (gv/vec2 0.15 0))
                          (g/unmap-point slider0 (gv/vec2 0.85 1)))
        inner (g/scale-size slider 0.95)]
    (svg/group {}
               slider
               (for [t (range 0 1 0.1)]
                 (gl/line2 (g/unmap-point slider (gv/vec2 0.0 t))
                           (g/unmap-point slider (gv/vec2 0.1 t))))
               (rect/rect (g/unmap-point inner (gv/vec2 0.1 (- pct 0.01)))
                          (g/unmap-point inner (gv/vec2 0.9 (+ pct 0.01)))))))

(defn vu-meter [center r pct]
  (let [p (tm/+ center (gv/vec2 0 (* 0.66 r)))
        t0 (* (/ 7 6) Math/PI)
        t1 (* (/ 11 6) Math/PI)
        inner (* 0.75 r)
        upper (* 0.9 r)
        lower (* 0.8 r)
        theta (tm/mix* t0 t1 pct)]
    (svg/group {}
               (csvg/path [[:M (tm/+ p (v/polar r t0))]
                           [:A [r r] 0 0 1 (tm/+ p (v/polar r t1))]
                           [:L (tm/+ p (v/polar inner t1))]
                           [:A [inner inner] 0 0 0 (tm/+ p (v/polar inner t0))]
                           [:Z]])
               (for [t (map #(+ % 0.05) (range t0 t1 0.1))]
                 (g/translate (gl/line2 (v/polar lower t) (v/polar upper t))
                              p))
               (gl/line2 (tm/+ p (v/polar (* 0.5 r) theta))
                         (tm/+ p (v/polar (* 0.95 r) theta)))
               ;; bounding box
               (with-meta (rect/rect (tm/+ center (gv/vec2 (- r) (* -0.5 r)))
                                     (tm/+ center (gv/vec2 (+ r) (* 0.5 r))))
                 {:rx 10}))))

(defn oscilliscope [center r]
  (let [rect (g/center (rect/rect (* 2 r)) center)]
    (svg/group {}
               (with-meta rect
                 {:rx 25})
               (for [t (range 0.1 0.9 0.2)]
                 (gl/line2 (g/unmap-point rect (gv/vec2 t 0.025))
                           (g/unmap-point rect (gv/vec2 t 0.975))))
               (for [t (range 0.1 0.9 0.2)]
                 (gl/line2 (g/unmap-point rect (gv/vec2 0.025 t))
                           (g/unmap-point rect (gv/vec2 0.975 t)))))))

(defn plug [p r]
  (svg/group {}
             (gc/circle p (* 0.75 r))
             (gc/circle p r)))

(defn knob [p r pct]
  (let [mapper (fn [t] (tm/map-interval t [0 1] [Math/PI (* 2.5 Math/PI)]))
        theta (mapper pct)
        w 0.08]
    (svg/group {}
               (gc/circle p (* 0.9 r))
               (for [t (range 0 1 0.1)]
                 (gl/line2 (tm/+ p (v/polar (* 0.90 r) (mapper t)))
                           (tm/+ p (v/polar (* 1 r) (mapper t)))))
               (with-meta (-> (rect/rect (gv/vec2 (* 0.4 r) (* (- w) r))
                                         (gv/vec2 (* 1.025 r) (* w r)))
                              (g/rotate theta)
                              (g/translate p))
                 {:rx 10 :fill "white"}))))

(defn divide-panels [{[w h] :size :as bounds}]
  (let [area-ratio (/ (g/area bounds) (g/area screen))]
    (if (dr/chance (cond (< area-ratio 0.1) 1
                         (> area-ratio 0.8) 0
                         :else (- 1.0 area-ratio)))
      [bounds]
      (let [div (fn [p q]
                  (fn [b]
                    [(gv/vec2 p) (gv/vec2 q) (if (dr/chance 0.2)
                                               (dr/rand-nth [:clockwise :counter-clockwise :all])
                                               (square/row-major b))]))
            divisions {(div [0.3 0.0] [0.7 1.0]) (if (> w h) 1 0)
                       (div [0.4 0.0] [1.0 0.5]) 1
                       (div [0.0 0.0] [0.6 0.5]) 1
                       (div [0.0 0.0] [0.5 0.3]) 1}]
        (mapcat (fn [s] (divide-panels s))
                (apply square/punch-out-relative bounds ((dr/weighted divisions) bounds)))))))

(defn assign-pane [{[w h] :size :as bounds}]
  (let [min-edge (min w h)
        area-ratio (/ (g/area bounds) (g/area screen))
        weights {:sliders 1
                 :vu-meter 1
                 :knobs 2
                 :plugs (if (< area-ratio 0.1)
                          0.8
                          0)
                 :oscilliscope (if (and (tm/delta= w h (* 0.33 min-edge))
                                        (> area-ratio 0.2)) 1 0.0)
                 :circles 0.5
                 :subdivide (if (< area-ratio 0.1) 0 area-ratio)}]
    (case (dr/weighted weights)
      :subdivide
      (mapcat assign-pane
              (let [splits (dr/weighted {2 2
                                         3 1})]
                (if (> w h)
                  (g/subdivide bounds {:rows 1 :cols splits})
                  (g/subdivide bounds {:rows splits :cols 1}))))
      :sliders
      (for [s (g/subdivide bounds {:rows 1 :cols (dr/random-int 2 5)})]
        (vertical-slider s (dr/random)))
      :knobs
      (for [s (g/subdivide bounds {:rows (dr/random-int 2 6) :cols (dr/random-int 3 5)})]
        (knob (g/centroid s) (* 0.08 min-edge) (dr/random)))
      :vu-meter
      (for [{[w1 h1] :size :as s}
            (let [n (dr/random-int 2 4)
                  opts (cond (> w (* 2 h)) {:rows 1 :cols n}
                             (> h (* 2 w)) {:rows n :cols 1}
                             :else {:rows 1 :cols 1})]
              (g/subdivide bounds opts))]
        (vu-meter (g/centroid s) (* 0.45 (min w1 h1)) (dr/random)))
      :oscilliscope
      (oscilliscope (g/centroid bounds) (* 0.45 min-edge))
      :plugs
      (let [size (* 0.05 (min width height))]
        (for [s (g/subdivide bounds {:rows (int (/ h size)) :cols (int (/ w size))})]
          (plug (g/centroid s) (* 0.3 size))))
      :circles
      (for [s (g/subdivide bounds {:rows 4 :cols 2})]
        (gc/circle (g/centroid s) (* 0.12 min-edge))))))

(defn shapes []
  (let [bounds (g/scale-size screen 0.975)
        panels (mapv (fn [s] (with-meta (g/scale-size s 0.95) {:rx 10}))
                     (divide-panels bounds))]
    (concat panels (mapcat assign-pane panels))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 0.75}
            (apply list (shapes))))

(sketch/definition control-panels
  {:created-at "2022-02-07"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :control-panels)
              "sketch-host"))
