(ns shimmers.sketches.terrain-grid
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.hexagon :as hex]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; TODO: look at https://www.redblobgames.com/grids/parts/ for how to calculate
;; adjoining edges. Also, look at how axial coordinates pretend to be grid
;; coordinates on that page unlike my generation algorithm here.

(defonce defo (debug/state))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn oddq-to-axial [col row]
  (let [q col
        r (- row (/ (- col (bit-and col 1)) 2))]
    [q r]))

(defn hex-grid [{p :p [width height] :size} cols rows]
  (let [size (min (/ width (* 1.5 (+ 0.5 cols)))
                  (/ height (* (Math/sqrt 3) (+ 0.5 rows))))
        w (* 2 size)
        h (* (Math/sqrt 3) size)
        base (tm/+ p
                   (gv/vec2 (* 0.5 (- width (* (/ 3 4) (- cols 1) w)))
                            (* 0.5 (- height (* (- rows 0.5) h)))))]
    (for [q (range cols)
          r (range rows)
          :let [axial (oddq-to-axial q r)
                center (tm/+ base (hex/axial-flat->pixel size axial))]]
      (assoc (hex/hexagon center size)
             :axial axial))))

(defn neighbors [index axial]
  (for [neighbor (hex/axial-range 1)
        :let [coord (tm/+ (gv/vec2 axial) neighbor)]
        :when (and (not= axial coord) (get index coord))]
    coord))

(defn hex-click
  [index {:keys [axial] :as hex}]
  #(swap! defo assoc
          :hex hex
          :neighbors (neighbors index axial)))

(defn hexagon [show-coords {:keys [p axial] :as hex} click]
  (csvg/group {:on-click click}
    (hex/flat-hexagon->polygon hex)
    (when show-coords
      (svg/text p
                (apply str (interpose "," axial))
                {:font-weight "normal"
                 :font-size "0.66em"
                 :stroke "none"
                 :fill "black"
                 :alignment-baseline "middle"
                 :text-anchor "middle"}))))

(defonce ui-state (ctrl/state {:size "16x10"
                               :debug {:coords true}}))

(def sizes {"8x5" [8 5]
            "16x10" [16 10]
            "32x20" [32 20]
            "64x20" [64 20]
            "64x40" [64 40]})

(defn shapes []
  (let [bounds (g/scale-size (rect/rect 0 0 width height) 0.98)
        show-coords (get-in @ui-state [:debug :coords])
        size (get sizes (get-in @ui-state [:size]))
        grid (apply hex-grid bounds size)
        index (into {} (for [c grid] [(:axial c) c]))]
    (for [cell grid]
      (hexagon show-coords cell (hex-click index cell)))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 1.0
             ;; required for on-click to fire on pointer events within group/polygon clip path
             :style {:pointer-events "fill"}}
    (shapes)))

(defn ui-controls []
  [:div.flexcols
   [:div {:style {:width "15em"}}
    [:h4 "Controls"]
    (ctrl/dropdown ui-state "Grid Size" [:size] (zipmap (keys sizes) (keys sizes)))
    (ctrl/checkbox ui-state "Show Coordinates" [:debug :coords])]
   [:div
    [:h4 "Debug"]
    (debug/display defo)]])

(sketch/definition terrain-grid
  {:created-at "2022-02-28"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :terrain-grid ui-controls)
              "sketch-host"))
