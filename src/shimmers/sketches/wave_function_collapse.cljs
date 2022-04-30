(ns shimmers.sketches.wave-function-collapse
  (:require
   [shimmers.algorithm.wave-function-collapse :as wfc]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(def color-map
  {"A" "#4444dd"
   "B" "#cdcd00"
   "C" "#228b22"})

(defn grid->cells [grid]
  (let [[cols rows] (:dims grid)
        w (/ width cols)
        h (/ height rows)]
    (for [j (range rows)
          i (range cols)]
      (let [values (get grid (gv/vec2 i j))
            cell (rect/rect (* w i) (* h j) w h)]
        (if (= (count values) 1)
          (vary-meta cell assoc :fill (get color-map (first values)))
          (->> (g/subdivide cell {:cols 2 :rows 2})
               (map (fn [color piece] (vary-meta piece assoc :fill color))
                    (map color-map values))
               (svg/group {})))))))

(def rule-a
  (wfc/str->matrix
   "AAA
    ABA
    AAA"))

(def rule-b
  (wfc/str->matrix
   "AAAAA
    ABBBA
    ABCBA
    ABBBA
    AAAAA"))

(def rule-c
  (wfc/str->matrix
   "AAAAAA
    ABBBBA
    ABCCBA
    ABCCBA
    ABBBBA
    AAAAAA"))

(def rule-d
  (wfc/str->matrix
   "AAAAAAAA
    AAAAAAAA
    AABBBBAA
    AABCCBAA
    AABCCBAA
    AABBBBAA
    AAAAAAAA
    AAAAAAAA"))

(defn shapes []
  (let [directions wfc/directions-8+4
        rt (wfc/rules (wfc/matrix->grid rule-c directions))]
    (grid->cells (wfc/solve (wfc/init-grid [40 30] directions (wfc/all-tiles rt))
                            rt))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "none"
             :fill "none"}
            (apply list (shapes))))

(sketch/definition wave-function-collapse
  {:created-at "2022-05-26"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :wave-function-collapse)
              "sketch-host"))
