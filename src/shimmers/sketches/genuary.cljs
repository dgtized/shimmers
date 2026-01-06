(ns shimmers.sketches.genuary
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn letter [region character]
  (let [box (g/scale-size region 0.9)]
    (csvg/group {}
      box
      (svg/text (rect/bottom-left box) character)
      (svg/text (rect/top-right box) character))))

(defn shapes [word bounds]
  (let [cgroup (g/center (first (g/subdivide (g/scale-size bounds 0.99) {:rows 3 :cols 1}))
                         (rv 0.5 0.5))
        letter-boxes
        (g/subdivide cgroup {:rows 1 :cols (count word)}) ]
    (mapv letter letter-boxes (seq word))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed
    {:id scene-id
     :width width
     :height height
     :stroke "black"
     :fill "none"
     :stroke-width 0.5}
    (shapes "Genuary"
            (csvg/screen width height))))

(defn explanation []
  [:div
   [:p "Genuary 2026 - Day5 - Genuary"]
   [:p "Ie write Genuary while avoiding a font."]])

(sketch/definition genuary
  {:created-at "2026-01-05"
   :tags #{:genuary2026}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args explanation scene)))
