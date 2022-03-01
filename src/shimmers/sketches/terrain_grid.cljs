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
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn hex-grid [{[w h] :size} rows cols]
  (let [radius (min (/ w cols) (/ h rows))]
    (into {}
          (for [q (range cols)
                r (range rows)
                :let [center (hex/axial-flat->pixel radius [q r])]]
            [[q r] (hex/hexagon center radius)]))))


(def flat-hex-angles (butlast (range 0 tm/TWO_PI (/ tm/TWO_PI 6))))

(defn hexagon->polygon [{:keys [p r]}]
  (-> (for [theta flat-hex-angles]
        (v/polar r theta))
      gp/polygon2
      (g/translate p)))

(defn shapes []
  (let [bounds (rect/rect 0 0 width height)]
    (map hexagon->polygon (vals (hex-grid bounds 6 4)))))

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
