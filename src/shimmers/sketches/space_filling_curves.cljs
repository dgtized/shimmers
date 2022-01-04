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

(defn moore-curve [depth]
  (let [axiom (seq "LFL+F+LFL")
        productions {"L" (seq "-RF+LFL+FR-")
                     "R" (seq "+LF-RFR-FL+")}]
    (remove (set (keys productions))
            (nth (iterate (fn [s] (mapcat #(get productions % %) s)) axiom) depth))))

(defn rewrite-turtle [pos orientation length rules]
  (svg/path (into [[:M pos]]
                  (keep (fn [[c p _]] (when (= c "F") [:L p]))
                        (reductions
                         (fn [[_ p o] r]
                           (case r
                             "F" ["F" (tm/+ p (tm/* o length)) o]
                             "+" ["+" p (left o)]
                             "-" ["-" p (right o)]))
                         ["" pos orientation] rules)))))

(defn up-line []
  [:up])

(defn down-line []
  [:down])

(defn left-line []
  [:left])

(defn right-line []
  [:right])

(defn hilbert-curve [n turn]
  (if (zero? n)
    []
    (case turn
      :up
      (concat (hilbert-curve (dec n) :right)
              (up-line)
              (hilbert-curve (dec n) :up)
              (right-line)
              (hilbert-curve (dec n) :up)
              (down-line)
              (hilbert-curve (dec n) :left))
      :left
      (concat (hilbert-curve (dec n) :down)
              (left-line)
              (hilbert-curve (dec n) :left)
              (down-line)
              (hilbert-curve (dec n) :left)
              (right-line)
              (hilbert-curve (dec n) :up))
      :right
      (concat (hilbert-curve (dec n) :up)
              (right-line)
              (hilbert-curve (dec n) :right)
              (up-line)
              (hilbert-curve (dec n) :right)
              (left-line)
              (hilbert-curve (dec n) :down))
      :down
      (concat (hilbert-curve (dec n) :left)
              (down-line)
              (hilbert-curve (dec n) :down)
              (left-line)
              (hilbert-curve (dec n) :down)
              (up-line)
              (hilbert-curve (dec n) :right)))))

(defn turtle [start-pos length curve]
  (reductions (fn [pos dir]
                (case dir
                  :up (tm/+ pos (gv/vec2 0 (- length)))
                  :down (tm/+ pos (gv/vec2 0 length))
                  :right (tm/+ pos (gv/vec2 length 0))
                  :left (tm/+ pos (gv/vec2 (- length) 0))))
              start-pos curve))

(defn turtle->path [positions]
  (svg/path
   (into [[:M (first positions)]]
         (mapv (fn [p] [:L p]) (rest positions)))))

(defn shapes [algorithm depth]
  (let [divider (dec (Math/pow 2 depth))
        length (/ width divider)]
    (case algorithm
      "moore"
      (rewrite-turtle (gv/vec2 (* 0.5 (dec divider) length) height)
                      (orientation :up) length
                      (moore-curve (dec depth)))
      "hilbert"
      (->> (hilbert-curve depth :up)
           (turtle (rv 0.0 1.0) length)
           turtle->path))))

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
