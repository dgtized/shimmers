(ns shimmers.sketches.genuary
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn letter-path [letter]
  (case letter
    "G" [[0.85 0.1] [0.15 0.1] [0.15 0.9] [0.85 0.9] [0.85 0.55] [0.65 0.6]]
    "e" [[0.3 0.55] [0.9 0.5] [0.5 0.25] [0.1 0.5] [0.5 0.9] [0.9 0.7]]
    "n" [[0.15 0.3] [0.15 0.9] [0.15 0.4] [0.5 0.3] [0.85 0.4] [0.85 0.9]]
    "u" [[0.15 0.3] [0.15 0.6] [0.25 0.9] [0.75 0.9] [0.85 0.6] [0.85 0.3] [0.85 0.9]]
    "a" [[0.15 0.3] [0.85 0.25] [0.85 0.9] [0.15 0.9] [0.15 0.45] [0.85 0.55]]
    ;; kerning!
    "r" [[0.2 0.9] [0.2 0.3] [0.2 0.425] [0.8 0.3]]
    "y" [[0.15 0.9] [0.85 0.3] (tm/mix (gv/vec2 0.15 0.9) (gv/vec2 0.85 0.3) 0.6)
         [0.25 0.3]]
    []))

;; TODO: add some inversion regions with white on black?
(defn render [box path]
  (let [translated (map (fn [p] (g/unmap-point box (gv/vec2 p))) path)
        strip (gl/linestrip2 translated)]
    (csvg/group {:stroke-width 1.5}
      (into [#_(csvg/path (csvg/segmented-path translated)
                          {:stroke-width 1 :fill "none"})]
            (mapv (fn [t]
                    (vary-meta
                     (let [circle (gc/circle (tm/+ (g/point-at strip t) (dr/randvec2 1.5))
                                             (dr/random 3.0 6.0))]
                       (case (dr/weighted {:triangle 1.0 :circle 1.0 :square 1.0})
                         :triangle (triangle/inscribed-equilateral circle (dr/random-tau))
                         :circle circle
                         :square (g/scale-size (g/bounds circle) 0.9)))
                     assoc :stroke-width (dr/random 1.0 2.0)))
                  (tm/norm-range 35))))))

(comment
  (render (rect/rect 0 0 10 10) (letter-path "a")))

(defn letter [region character]
  (let [box (g/translate (g/scale-size region 0.9) (dr/randvec2 4))]
    (csvg/group {}
      [#_box
       (render box (letter-path character))
       #_(svg/text (rect/bottom-left box) character)
       #_(svg/text (rect/top-right box) character)])))

(defn word-paths [bounds word]
  (let [cgroup (g/center (first (g/subdivide (g/scale-size bounds 0.99) {:rows 3 :cols 1}))
                         (rv 0.5 0.5))
        letter-boxes (g/subdivide cgroup {:rows 1 :cols (count word)})]
    (mapv letter letter-boxes (seq word))))

(defn shapes [bounds word]
  (word-paths bounds word))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed
    {:id scene-id
     :width width
     :height height
     :stroke "black"
     :fill "none"
     :stroke-width 0.5}
    (shapes (csvg/screen width height)
            "Genuary")))

(defn explanation []
  [:div
   [:p "Genuary 2026 - Day5 - Genuary"]
   [:p "Ie write Genuary while avoiding a font."]])

(sketch/definition genuary
  {:created-at "2026-01-05"
   :tags #{:genuary2026}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args explanation scene)))
