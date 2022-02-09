(ns shimmers.sketches.negative-overlap
  (:require
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.algorithm.square-packing :as square]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.color :as color]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn random-rect [scale]
  (let [scaled (fn [v side] (int (* (tm/roundto v (/ 1 scale)) side)))
        w (dr/random 0.1 0.9)
        h (dr/random 0.1 0.9)
        x (dr/random (- 1 w))
        y (dr/random (- 1 h))]
    (assoc (rect/rect (scaled x width) (scaled y height)
                      (scaled w width) (scaled h height))
           :open (inc (dr/random-int 15)))))

(defn big-enough? [rect]
  (let [{[w h] :size} rect]
    (and (> w 20) (> h 20))))

(def base-shape
  (assoc (rect/rect 0 0 width height) :open 2r0000))

(defn assign-open [shapes open]
  (map #(assoc % :open open) shapes))

(defn xor-open [{a :open} {b :open}]
  (bit-xor a b))

(defn rect= [{:keys [p size]} {pb :p sizeb :size}]
  (and (tm/delta= p pb) (tm/delta= size sizeb)))

(defn translated-panes [{pa :p :as a} b]
  (->> (square/surrounding-panes (g/translate a (tm/- pa))
                                 (g/translate b (tm/- pa))
                                 (dr/weighted {(square/row-major a) 5
                                               :all 1}))
       (map #(g/translate % pa))
       (filter (fn [{[w h] :size}] (and (> w 1) (> h 1))))))

;; for now this is removing the clip each time
(defn rect-exclusion [a b]
  (let [clip (g/clip-with (g/as-polygon a) (g/as-polygon b))]
    ;; no intersection between pair causes g/clip-with to
    ;; return (gp/polygon2 [])
    (if (empty? (:points clip))
      [a]
      (let [clip (g/bounds clip)]
        (cond (rect= clip b) ;; b is contained by a
              (concat (assign-open (translated-panes a b) (:open a))
                      (assign-open [clip] (xor-open a b)))
              (rect= clip a) ;; a is contained by b
              ;; FIXME: This is probably causing a bug if b also clips other
              ;; shapes? partial fix to ignore existing panes, but *should*
              ;; re-clip remainder of a?
              (concat #_(assign-open (translated-panes b a) (:open b))
                      (assign-open [clip] (xor-open a b)))
              :else ;; partial overlap
              (concat (assign-open (translated-panes a clip) (:open a))
                      (assign-open [clip] (xor-open a b))))))))

;; Add example with triple overlap, with weird near zero width/height slivers
;; http://localhost:9500/#/sketches/negative-overlap?seed=3862608476

;; FIXME: ignoring any remainder of shape that did not intersect anything
(defn add-split-shapes [shapes s]
  (let [isec? (fn [x] (g/intersect-shape (g/bounds s) (g/bounds x)))
        intersections (filter isec? shapes)
        disjoint (remove isec? shapes)]
    (concat disjoint (mapcat #(rect-exclusion % s) intersections))))

(defn fill-shape [mode palette {:keys [open] :as shape}]
  (if (or (= mode :color) (not (bit-test open 3)))
    (vary-meta shape assoc :fill (nth palette (bit-and 2r0011 open)))
    shape))

(defn hatch-shapes [mode {:keys [open] :as shape}]
  (if (or (= mode :hatch) (bit-test open 3))
    (let [n (mod open 8)]
      (clip/hatch-rectangle shape
                            (tm/map-interval n [0 8] [3 8])
                            (+ 0.05 (* (/ n 8) tm/TWO_PI))
                            [0 0]))
    []))

(defn random-additions [n]
  (->> (partial random-rect 25)
       repeatedly
       (filter big-enough?)
       (take n)))

(def palettes
  (->> ["https://artsexperiments.withgoogle.com/artpalette/colors/f2f1f1-959cac-4972a6-2d3447"
        "https://artsexperiments.withgoogle.com/artpalette/colors/e4ddc8-c8657a-5d554d-c7af9f"
        "https://artsexperiments.withgoogle.com/artpalette/colors/d4d1ce-715439-a9895e-cfb08a"
        "https://artsexperiments.withgoogle.com/artpalette/colors/edece8-94928b-4f5963-94655f"]
       (mapv color/url->hex-colors)))

(defn shapes [mode]
  (let [palette (dr/rand-nth palettes)
        additions (random-additions 7)
        shapes (reduce add-split-shapes [base-shape] additions)]
    [(svg/group {} shapes)
     (when (#{:mixed :color} mode)
       (svg/group {} (map (partial fill-shape mode palette) shapes)))
     (when (#{:mixed :hatch} mode)
       (svg/group {:stroke-width 0.5} (mapcat (partial hatch-shapes mode) shapes)))
     (svg/group {:stroke-width 2.0 :fill "#000"}
                (mapcat (fn [r] (map #(svg/circle % 2) (g/vertices r))) additions))]))

(def modes [:mixed :color :hatch])
(defonce ui-state (ctrl/state {:mode :mixed}))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 2.0}
            (apply list (shapes (:mode @ui-state)))))

;; TODO: embed this in the url somehow?
(defn ui-controls []
  [:div (ctrl/change-mode ui-state modes)])

(sketch/definition negative-overlap
  {:created-at "2022-01-26"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/with-controls scene :negative-overlap ui-controls)
              "sketch-host"))
