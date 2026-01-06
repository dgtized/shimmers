(ns shimmers.sketches.genuary
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn letter-path [letter]
  (println letter)
  (case letter
    "G" [[0.9 0.1] [0.1 0.1] [0.1 0.9] [0.9 0.9] [0.9 0.55] [0.65 0.6]]
    "e" [[0.3 0.55] [0.9 0.5] [0.5 0.25] [0.1 0.5] [0.5 0.9] [0.9 0.7]]
    "n" [[0.1 0.3] [0.1 0.9] [0.1 0.4] [0.5 0.3] [0.9 0.4] [0.9 0.9]]
    "u" [[0.1 0.3] [0.1 0.6] [0.2 0.9] [0.8 0.9] [0.9 0.6] [0.9 0.3] [0.9 0.9]]
    "a" [[0.1 0.3] [0.9 0.25] [0.9 0.9] [0.1 0.9] [0.1 0.45] [0.9 0.55]]
    "r" [[0.15 0.9] [0.15 0.3] [0.15 0.425] [0.8 0.3]]
    "y" [[0.1 0.9] [0.9 0.3] (tm/mix (gv/vec2 0.1 0.9) (gv/vec2 0.9 0.3) 0.6)
         [0.2 0.3]]
    []))

(defn render [box path]
  (let [translated (map (fn [p] (g/unmap-point box (gv/vec2 p))) path)]
    (csvg/path (csvg/segmented-path translated)
               {:stroke-width 2 :fill "none"})))

(render (rect/rect 0 0 10 10) (letter-path "a"))

(defn letter [region character]
  (let [box (g/scale-size region 0.9)]
    (csvg/group {}
      [box
       (render box (letter-path character))
       #_(svg/text (rect/bottom-left box) character)
       #_(svg/text (rect/top-right box) character)])))

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
