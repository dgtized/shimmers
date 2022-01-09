(ns shimmers.sketches.space-filling-curves
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.math.core :as tm]))

;; TODO: space-filling-curves can be used to offset map into a texture with
;; interesting locality properties. It can map a 2d coordinate to the closest
;; point on sfc(s) where s is a value from 0 -> 1 representing distance along
;; the curve. Likewise, for the reverse, from length s to a point in 2d space.

;; TODO: remove individual segments as an inverse greyscale map? Ie instead of
;; using a space-filling-curve to assist dithering, use the curve *as* the
;; representational medium and then dither which pixels are on?

(def width 800)
(def height 800)

;; https://en.wikipedia.org/wiki/Moore_curve
;; http://people.cs.aau.dk/~normark/prog3-03/html/notes/fu-intr-2_themes-hilbert-sec.html

(defn l-system [axiom productions]
  (fn [depth]
    (let [systems (iterate (fn [s] (mapcat #(get productions % %) s)) axiom)]
      (remove (set (keys productions)) (nth systems depth)))))

(def moore-curve
  (l-system (seq "LFL+F+LFL")
            {"L" (seq "-RF+LFL+FR-")
             "R" (seq "+LF-RFR-FL+")}))

(def hilbert-curve
  (l-system (seq "A")
            {"A" (seq "+BF-AFA-FB+")
             "B" (seq "-AF+BFB+FA-")}))

(defn rewrite-turtle [pos orientation length rules]
  (->> rules
       (reductions
        (fn [[_ p o] r]
          (case r
            "F" ["F" (tm/+ p (tm/* o length)) o]
            "+" ["+" p (v/turn-right o)]
            "-" ["-" p (v/turn-left o)]))
        ["" pos orientation])
       (keep (fn [[c p _]] (when (= c "F") p)))))

(defn rewrite-path [pos orientation length expansions]
  (->> expansions
       (rewrite-turtle pos orientation length)
       (mapv (fn [p] [:L p]))
       (into [[:M pos]])
       svg/path))

(defn shapes [algorithm depth]
  (let [divider (dec (Math/pow 2 depth))
        length (/ width divider)]
    (case algorithm
      "moore"
      (rewrite-path (v/vec2 (* 0.5 (dec divider) length) 0)
                    v/up length
                    (moore-curve (dec depth)))
      "hilbert"
      (rewrite-path (v/vec2)
                    v/up length
                    (hilbert-curve (inc depth))))))

(defn scene [algorithm depth]
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 1.0}
            (shapes algorithm depth)))

(defonce ui-state (ctrl/state {:algorithm "hilbert"
                               :depth 6}))

(defn page []
  [:div
   (let [{:keys [algorithm depth]} @ui-state]
     [:div.canvas-frame [scene algorithm depth]])
   [:div#interface
    (ctrl/container
     (ctrl/dropdown ui-state "Algorithm" [:algorithm]
                    {"moore" "moore"
                     "hilbert" "hilbert"})
     (ctrl/slider ui-state (fn [depth] (str "Depth " depth)) [:depth] [1 8 1]))]])

(sketch/definition space-filling-curves
  {:created-at "2022-01-02"
   :type :svg
   :tags #{:static}}
  (ctrl/mount page "sketch-host"))
