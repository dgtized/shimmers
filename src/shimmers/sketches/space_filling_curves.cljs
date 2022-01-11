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

(defn rewrite-curve [{:keys [large-arc sweep-flag radius-div]}]
  (fn [pos orientation length expansions]
    (let [radius (/ length (case radius-div
                             "0.5" 0.5
                             "1" 1
                             "sqrt2" (Math/sqrt 2)
                             "phi" tm/PHI
                             "2" 2))]
      (->> expansions
           (rewrite-turtle pos orientation length)
           (mapv (fn [p] [:A [radius radius]
                         0.0
                         (if large-arc 1 0)
                         (if sweep-flag 1 0)
                         p]))
           (into [[:M pos]])
           svg/path))))

(defn shapes [algorithm depth curved]
  (let [divider (Math/pow 2 depth)
        length (/ width divider)
        pathing (if (:enabled curved)
                  (rewrite-curve curved)
                  rewrite-path)]
    (case algorithm
      "moore"
      (pathing (v/vec2 (* 0.5 (dec divider) length) (* 0.5 length))
               v/up length
               (moore-curve (dec depth)))
      "hilbert"
      (pathing (v/vec2 (- width (/ length 2)) (/ length 2))
               v/left length
               (hilbert-curve depth)))))

(defn scene [algorithm depth curved]
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 1.0}
            (shapes algorithm depth curved)))

(defonce ui-state (ctrl/state {:algorithm "hilbert"
                               :curved {:enabled false
                                        :large-arc false
                                        :sweep-flag false
                                        :radius-div "sqrt2"}
                               :depth 6}))

(defn page []
  (let [{:keys [algorithm depth curved]} @ui-state]
    [:div
     [:div.canvas-frame [scene algorithm depth curved]]
     [:div#interface
      (ctrl/container
       (ctrl/dropdown ui-state "Algorithm" [:algorithm]
                      {"moore" "moore"
                       "hilbert" "hilbert"})
       (ctrl/slider ui-state (fn [depth] (str "Depth " depth)) [:depth] [1 8 1])
       (ctrl/checkbox ui-state "Curved" [:curved :enabled])
       (when (:enabled curved)
         [:div
          (ctrl/dropdown ui-state "Radius Divider" [:curved :radius-div]
                         {"0.5" "0.5"
                          "1" "1"
                          "sqrt2" "sqrt2"
                          "phi" "phi"
                          "2" "2"})
          (ctrl/checkbox ui-state "Large Arc" [:curved :large-arc])
          (ctrl/checkbox ui-state "Sweep Flag" [:curved :sweep-flag])]))]]))

(sketch/definition space-filling-curves
  {:created-at "2022-01-02"
   :type :svg
   :tags #{:static}}
  (ctrl/mount page "sketch-host"))
