(ns shimmers.sketches.displacements-inbetween
  (:require
   [shimmers.algorithm.lines :as lines]
   [shimmers.common.palette :as palette]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug :include-macros true]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.bezier :as bezier]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce defo (debug/state))
(def width 1024)
(def height 768)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn make-line [a b controls scale]
  (let [perpendicular (tm/normalize (g/normal (tm/- b a)) scale)]
    (-> (concat [a]
                (for [t (cs/midsection (tm/norm-range (inc controls)))]
                  (tm/+ (tm/mix a b t)
                        (tm/* perpendicular (dr/random -1 1))))
                [b])
        bezier/auto-spline2
        (g/sample-uniform (* 0.01 height) true)
        lines/indexed-line-strip)))

(def spacing-divisions
  {5 1
   7 2
   11 3
   13 4
   17 4
   19 3
   23 2
   29 1})

(defn base-lines []
  (let [simplify (fn [line] (lines/simplify-line line (* 0.0002 width)))
        angle (if (dr/chance 0.1)
                (dr/random -0.3 0.3)
                (dr/random -0.15 0.15))
        a (-> (make-line (rv 0.1 0.1) (rv 0.1 0.9) 2 (* 0.08 width))
              (g/rotate (dr/random -0.05 0.1))
              simplify
              (vary-meta assoc :stroke-width 2.0))
        b (-> (make-line (rv 0.5 0.0) (rv 0.5 1.0) 3 (* 0.12 width))
              (g/rotate (dr/random -0.05 0.05))
              simplify
              (vary-meta assoc :stroke-width 2.0))
        c (-> (make-line (rv 0.9 0.1) (rv 0.9 0.9) 2 (* 0.08 width))
              (g/rotate angle)
              simplify
              (vary-meta assoc :stroke-width 2.0))
        [n1 n2] (repeatedly 2 #(dr/weighted (dissoc spacing-divisions 5 7)))]
    (swap! defo assoc :base [angle n1 n2])
    (concat [a]
            (for [t (tm/norm-range n1)]
              (simplify (lines/mix-line a b t)))
            [b]
            (for [t (tm/norm-range n2)]
              (simplify (lines/mix-line b c t)))
            [c])))

(defn spaced
  [pair]
  (->> (dr/weighted spacing-divisions)
       tm/norm-range
       (mapv (fn [t] (lines/connect pair t)))))

(defn box-strip [pair offsets]
  (for [[t0 t1] (partition 2 1 offsets)]
    (lines/box-between pair t0 t1)))

(defn mirror-over [len p]
  (let [mid (int (* p len))]
    (fn [i x] [(if (< i mid)
                (- mid i)
                (- i mid)) x])))

(comment
  (let [xs (map char (range 97 123))]
    (map-indexed (mirror-over (count xs) 0.5) xs)))

(defn color-strip [palette pair]
  (let [n (dr/weighted spacing-divisions)
        boxes (box-strip pair (tm/norm-range n))
        palette-size (dr/rand-nth [2 3 4 5 7 9 11])
        palette-seq (repeatedly palette-size #(dr/rand-nth palette))
        index-op (if (dr/chance (/ palette-size 12))
                   (mirror-over (count boxes) 0.5)
                   vector)]
    (for [[i cell] (map-indexed index-op boxes)]
      (vary-meta cell assoc :fill (nth palette-seq (mod i palette-size))))))

(defn lines [palette]
  (let [lines (->> (base-lines)
                   #_(debug/time-it defo [:time :base-lines]))
        pairs (partition 2 1 lines)
        percent-color (dr/weighted {0.01 1.0
                                    0.05 3.0
                                    0.10 3.0
                                    0.15 2.0
                                    0.20 1.0
                                    0.25 0.5})]
    (swap! defo assoc :percent-color percent-color)
    (concat (dr/map-random-sample (constantly 0.1)
                                  (fn [line] (vary-meta line assoc :stroke-width (dr/random 3 8)))
                                  lines)
            (->> pairs
                 (dr/random-sample 0.5)
                 (mapcat spaced)
                 (dr/random-sample 0.85)
                 #_(debug/time-it defo [:time :spacing]))
            (->> pairs
                 (dr/random-sample percent-color)
                 (mapcat (partial color-strip palette))
                 #_(debug/time-it defo [:time :color-strip])))))

(defn fit-region
  "fit-all-into-bounds removes the meta attribs in copy, so add them back."
  [bounds shapes]
  (mapv (fn [shape fit] (with-meta fit (meta shape)))
        shapes
        (gu/fit-all-into-bounds bounds shapes)))

;; TODO: improve palette selection
(def palettes
  (->> ["https://artsexperiments.withgoogle.com/artpalette/colors/395054-0c0c15-a35192-3ca6a8-d8ead8" ;; pink/teal
        "https://artsexperiments.withgoogle.com/artpalette/colors/f2bc46-95968d-304e5a-633e15-bd8036" ;; yellow/blue/brown
        "https://artsexperiments.withgoogle.com/artpalette/colors/2f403d-e9e6d9-b4533a-9b9270-ddbd67" ;; red/yellow
        "https://artsexperiments.withgoogle.com/artpalette/colors/adc7e5-e1e6e7-5087ba-b89474-222982" ;; blue/tan
        "https://artsexperiments.withgoogle.com/artpalette/colors/4f8e98-53ad74-aa93b7-e2eae9-415e98" ;; cyan/green/purple/blue
        ]
       palette/from-urls
       (concat [["maroon" "gold" "black"]])))

(defn scene [screen palette]
  (let [shapes (->> (lines (dr/shuffle (into palette (repeat 2 "white"))))
                    (fit-region screen)
                    #_(debug/time-it defo [:time :generate]))]
    (csvg/timed
     (csvg/svg {:width width
                :height height
                :stroke "black"
                :stroke-opacity 0.8
                :stroke-width 1.0}
       shapes))))

(defn page []
  (let [screen (g/scale-size (rect/rect 0 0 width height) 0.95)
        palette (dr/rand-nth palettes)]
    [:<>
     [:div.canvas-frame [scene screen palette]]
     [:div.contained
      [:div.flexcols {:style {:justify-content :space-evenly :align-items :center}}
       [view-sketch/generate :displacements-inbetween]
       [palette/as-svg {:width 250 :height 12} palette]]
      #_(debug/pre-edn @defo)]]))

(sketch/definition displacements-inbetween
  {:created-at "2021-11-13"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount page))
