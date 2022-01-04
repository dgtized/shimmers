(ns shimmers.sketches.space-filling-curves
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; TODO: space-filling-curves can be used to offset map into a texture with
;; interesting locality properties. It can map a 2d coordinate to the closest
;; point on sfc(s) where s is a value from 0 -> 1 representing distance along
;; the curve. Likewise, for the reverse, from length s to a point in 2d space.

(def width 800)
(def height 800)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

;; https://en.wikipedia.org/wiki/Moore_curve
;; http://people.cs.aau.dk/~normark/prog3-03/html/notes/fu-intr-2_themes-hilbert-sec.html

(defn orientation [dir]
  (case dir
    :up (gv/vec2 0 -1)
    :down (gv/vec2 0 1)
    :right (gv/vec2 1 0)
    :left (gv/vec2 -1 0)))

(defn right [[x y]]
  (gv/vec2 y (- x)))

(defn left [[x y]]
  (gv/vec2 (- y) x))

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
            "+" ["+" p (left o)]
            "-" ["-" p (right o)]))
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
      (rewrite-path (gv/vec2 (* 0.5 (dec divider) length) height)
                    (orientation :up) length
                    (moore-curve (dec depth)))
      "hilbert"
      (rewrite-path (rv 0.0 1.0)
                    (orientation :up) length
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
