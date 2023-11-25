(ns shimmers.sketches.radial-wings
  (:require
   [shimmers.common.palette :as palette]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn rp [r theta]
  (v/polar (* r 0.45 height) theta))


(def palettes
  (palette/by-names
   [:purple-shell-brown
    :shell-aqua-blue-green
    :slate-shell-red-tan-yellow
    :shell-grey-blues-bold
    :yellow-blue-slate-grey-red
    :red-black-yellow-grey-blue
    :orange-black-blue-shell-red
    :blues-orange-black-shell]))

(defn shapes [palette spin [d0 d1]]
  (->> (for [[a b] (->> (dr/density-range d0 d1 true)
                        (partition 2 1))
             :when (> (- b a) d0)
             :let [[ra rb] (map (partial + 0.01) [a b])]]
         (gp/polygon2 [(rp 0 0)
                       (rp ra (+ spin (* a tm/TWO_PI)))
                       (rp rb (+ spin (* b tm/TWO_PI)))]))
       (map-indexed (fn [i s] (vary-meta (g/translate s (rv 0.5 0.5)) assoc
                                        :fill (nth palette (mod i (count palette))))))))

(defn scene [palette]
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.5}
    (shapes (dr/shuffle palette) (dr/random-tau)
            (dr/rand-nth [[0.01 0.03]
                          [0.01 0.05]
                          [0.02 0.08]
                          [0.03 0.1]]))))

(defn page []
  (let [palette (:colors (dr/rand-nth palettes))]
    (fn []
      [:<>
       [:div.canvas-frame [scene palette]]
       [:div.contained
        [:div.evencols
         [view-sketch/generate :radial-wings]
         [palette/as-svg {} palette]]]])))

(sketch/definition radial-wings
  {:created-at "2021-11-15"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount page))
