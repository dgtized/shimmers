(ns shimmers.sketches.space-filling-curves
  (:require
   [clojure.set :as set]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
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
;; TODO: https://en.wikipedia.org/wiki/Z-order_curve
;; See also: https://onlinemathtools.com/l-system-generator

(defn l-system [{:keys [axiom rules]}]
  (let [products (cs/map-kv seq rules)]
    (fn [depth]
      (as-> (seq axiom) system
        (iterate (fn [s] (mapcat #(get products % %) s)) system)
        (nth system depth)
        (remove (set (keys products)) system)))))

(def rule-systems
  [{:name "Moore Curve"
    :axiom "LFL+F+LFL"
    :rules {"L" "-RF+LFL+FR-"
            "R" "+LF-RFR-FL+"}
    :orientation v/up
    :start (fn [depth]
             (let [divider (Math/pow 2 depth)
                   length (/ width divider)]
               {:pos (v/vec2 (* 0.5 (dec divider) length) (* 0.5 length))
                :length length}))}

   {:name "Hilbert Curve"
    :axiom "+BF-AFA-FB+" ;; "A"
    :rules {"A" "+BF-AFA-FB+"
            "B" "-AF+BFB+FA-"}
    :orientation v/left
    :start (fn [depth]
             (let [divider (Math/pow 2 depth)
                   length (/ width divider)]
               {:pos (v/vec2 (- width (/ length 2)) (/ length 2))
                :length length}))}

   {:name "Sierpinsky Square"
    :axiom "F+XF+F+XF"
    :rules {"X" "XF-F+F-XF+F+XF-F+F-X"}
    :orientation (g/rotate v/up (/ (- tm/TWO_PI) 8))
    :start (fn [depth]
             (let [divider (Math/pow 2 depth)
                   length (/ width divider)]
               {:pos (v/vec2 (* (/(Math/sqrt 2) 3) length) (- height length))
                :length (/ length (Math/sqrt 2))}))}])

(defn by-name [n]
  (first ((set/index rule-systems [:name]) {:name n})))

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

(defn rewrite-path [path]
  (mapv (fn [p] [:L p]) path))

(defn rewrite-curve [{:keys [large-arc sweep-flag radius-div]} length]
  (fn [path]
    (let [radius (/ length (case radius-div
                             "0.5" 0.5
                             "1/sqrt2" (/ 1 (Math/sqrt 2))
                             "1" 1
                             "sqrt2" (Math/sqrt 2)
                             "phi" tm/PHI
                             "1.9" 1.9
                             "2" 2))]
      (mapv (fn [p] [:A [radius radius]
                    0.0
                    (if large-arc 1 0)
                    (if sweep-flag 1 0)
                    p])
            path))))

(defn rewrite-quad-bezier [path]
  (->> path
       (partition 3 2)
       (mapcat (fn [[a b c]] [[:M a] [:T b] [:T c]]))))

(defn shapes [system depth curved]
  (let [{:keys [pos length]} ((:start system) depth)
        rewrite-segments (case (:mode curved)
                           "arcs" (rewrite-curve curved length)
                           "quad-beziers" rewrite-quad-bezier
                           "lines" rewrite-path)]
    (->> ((l-system system) (dec depth))
         (rewrite-turtle pos (:orientation system) length)
         rewrite-segments
         (into [[:M pos]])
         csvg/path)))

(defn scene [rule-name depth curved]
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 1.0}
            (shapes (by-name rule-name) depth curved)))

(defonce ui-state
  (ctrl/state {:rule-system (:name (first rule-systems))
               :curved {:mode "lines"
                        :large-arc false
                        :sweep-flag false
                        :radius-div "sqrt2"}
               :depth 6}))

(defn controls []
  (let [systems (into {} (map (fn [{:keys [name]}] [name name]) rule-systems))
        {:keys [curved]} @ui-state]
    (ctrl/container
     (ctrl/dropdown ui-state "Rule System" [:rule-system] systems)
     (ctrl/slider ui-state (fn [depth] (str "Depth " depth)) [:depth] [1 8 1])
     (ctrl/dropdown ui-state "Display" [:curved :mode]
                    {"Lines" "lines"
                     "Arcs" "arcs"
                     "Quad-Beziers" "quad-beziers"})
     (when (= (:mode curved) "arcs")
       [:div
        (ctrl/dropdown ui-state "Radius Divider" [:curved :radius-div]
                       {"0.5" "0.5"
                        "1/sqrt2" "1/sqrt2"
                        "1" "1"
                        "sqrt2" "sqrt2"
                        "phi" "phi"
                        "1.9" "1.9"
                        "2" "2"})
        (ctrl/checkbox ui-state "Large Arc" [:curved :large-arc])
        (ctrl/checkbox ui-state "Sweep Flag" [:curved :sweep-flag])]))))

(defn page []
  (let [{:keys [rule-system depth curved]} @ui-state]
    [:div
     [:div.canvas-frame [scene rule-system depth curved]]
     [:div.flexcols
      [:div#interface [controls]]
      (debug/pre-edn (dissoc (by-name rule-system) :start))]]))

(sketch/definition space-filling-curves
  {:created-at "2022-01-02"
   :type :svg
   :tags #{:static}}
  (ctrl/mount page "sketch-host"))
