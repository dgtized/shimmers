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
    (into {}
          (for [q (range cols)
                r (range rows)
                :let [axial (oddq-to-axial q r)
                      center (tm/+ base (hex/axial-flat->pixel size axial))]]
            [axial (assoc (hex/hexagon center size)
                          :axial axial)]))))

(defn hexagon [show-coords {:keys [p axial] :as hex}]
  (svg/group {:on-click #(swap! defo assoc :hex hex)}
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

(defonce ui-state (ctrl/state {:debug {:coords true}}))

(defn shapes []
  (let [bounds (g/scale-size (rect/rect 0 0 width height) 0.98)
        show-coords (get-in @ui-state [:debug :coords])]
    (map (partial hexagon show-coords) (vals (hex-grid bounds 16 10)))))

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
   [:div (ctrl/checkbox ui-state "Show Coordinates" [:debug :coords])]
   (debug/display defo)])

(sketch/definition terrain-grid
  {:created-at "2022-02-28"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :terrain-grid ui-controls)
              "sketch-host"))
