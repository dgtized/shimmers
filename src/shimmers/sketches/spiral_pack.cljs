(ns shimmers.sketches.spiral-pack
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn spiral-step [{r :r dr :dr t :t}]
  (let [dr' (* 1.01 dr)
        dt (Math/atan2 (+ dr dr') (+ r dr))]
    {:t (+ t dt)
     :r (+ r (* dr' 0.1))
     :dr (* 1.03 dr')
     :circle (gc/circle (v/+polar (rv 0.5 0.5) (+ dr r) t)
                        dr)}))

(defn shapes []
  (let [radius (* height 0.05)
        circle (gc/circle (rv 0.5 0.5) radius)]
    (->> {:circle circle :t 0 :r radius :dr (* 0.3 radius)}
         (iterate spiral-step)
         (take 60)
         (map :circle))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 0.5}
    (shapes)))

(sketch/definition spiral-pack
  {:created-at "2022-03-13"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :spiral-pack)
              "sketch-host"))
