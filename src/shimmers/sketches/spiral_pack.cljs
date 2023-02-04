(ns shimmers.sketches.spiral-pack
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(def screen (rect/rect 0 0 width height))

(defn spiral-surrounding []
  (let [radius (* height 0.05)
        circle (gc/circle (rv 0.5 0.5) radius)]
    (->> {:circle circle :t 0 :r radius :dr (* 0.3 radius)}
         (iterate
          (fn [{r :r dr :dr t :t}]
            (let [dr' (* 1.005 dr)
                  dt (Math/atan2 (+ dr dr') (+ r dr))]
              {:t (+ t dt)
               :r (+ r (* dr' 0.1))
               :dr (* 1.03 dr')
               :circle (gc/circle (v/+polar (rv 0.5 0.5) (+ dr r) t)
                                  dr)})))
         (take-while (fn [{:keys [circle]}] (collide/bounded? screen circle)))
         (map :circle))))

(defn spiral-inside []
  (let [radius (* height 0.45)
        center (rv 0.5 0.5)
        dr (dr/random 0.9 0.99)
        dt (dr/random 0.05 0.30)
        circle (gc/circle center radius)]
    (println [dr dt])
    (->> {:circle circle :t (dr/random-tau) :r radius}
         (iterate
          (fn [{:keys [circle t r]}]
            (let [r' (* 0.98 r)
                  radial-pt (g/point-at circle (/ t eq/TAU))]
              {:circle (gc/circle (v/+polar radial-pt r' (- t Math/PI)) r')
               :t (+ t dt)
               :r r'})))
         (take-while (fn [{:keys [r]}] (> r 4.0)))
         (map :circle))))

(defn box-pack []
  (let [size (* height 0.95)
        scale 0.9
        base (-> (rect/rect size)
                 (g/center)
                 (g/translate (rv 0.5 0.5)))]
    (->> {:r base :i 0}
         (iterate
          (fn [{:keys [r i] :as s}]
            (let [{p :p} r
                  r' (g/scale-size r scale)
                  {p' :p} r'
                  move-ul (tm/- p p')
                  move-lr (tm/- p' p)
                  move
                  (case (mod i 4)
                    0 move-ul
                    1 (gv/vec2 (:x move-lr) (:y move-ul))
                    2 move-lr
                    3 (gv/vec2 (:x move-ul) (:y move-lr)))]
              (-> s
                  (assoc :r (g/translate r' (tm/* move 0.66)))
                  (update :i inc)))))
         (take-while (fn [{:keys [r]}] (> (g/area r) 10.0)))
         (map :r))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 0.5}
    ((dr/weighted {spiral-inside 3
                   spiral-surrounding 1
                   box-pack 1}))))

(sketch/definition spiral-pack
  {:created-at "2022-03-13"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :spiral-pack)
              "sketch-host"))
