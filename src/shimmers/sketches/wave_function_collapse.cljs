(ns shimmers.sketches.wave-function-collapse
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [shimmers.algorithm.wave-function-collapse :as wfc]
   [thi.ng.geom.rect :as rect]))

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
      (vary-meta (rect/rect (* w i) (* h j) w h)
                 merge {:fill
                        (let [values (get grid (gv/vec2 i j))]
                          (if (= (count values) 1)
                            (get color-map (first values))
                            "#dd4444"))}))))

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
  (let [directions wfc/directions-8
        rt (wfc/rules (wfc/matrix->grid rule-d directions))]
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
