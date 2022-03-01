(ns shimmers.sketches.terrain-grid
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.hexagon :as hex]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn oddq-to-axial [col row]
  (let [q col
        r (- row (/ (- col (bit-and col 1)) 2))]
    [q r]))

(defn hex-grid [{[w h] :size} rows cols]
  (let [size (min (/ w (* 2 cols))
                  (/ h (* (Math/sqrt 3) rows)))
        base (gv/vec2 size (* 0.5 (* size (Math/sqrt 3))))]
    (into {}
          (for [q (range rows)
                r (range cols)
                :let [axial (oddq-to-axial q r)
                      center (hex/axial-flat->pixel size axial)]]
            [axial (assoc (hex/hexagon (tm/+ center base) size)
                          :axial axial)]))))

(def flat-hex-angles (butlast (range 0 tm/TWO_PI (/ tm/TWO_PI 6))))

(defn hexagon->polygon [{:keys [p r axial]}]
  (let [hex (-> (for [theta flat-hex-angles]
                  (v/polar r theta))
                gp/polygon2
                (g/translate p))
        text (svg/text p
                       (apply str (interpose "," axial))
                       {:fill "black"
                        :alignment-baseline "middle"
                        :text-anchor "middle"})]
    (svg/group {} hex text)))

(defn shapes []
  (let [bounds (rect/rect 0 0 width height)]
    (map hexagon->polygon (vals (hex-grid bounds 4 3)))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 1.0}
            (shapes)))

(sketch/definition terrain-grid
  {:created-at "2022-"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :terrain-grid)
              "sketch-host"))
