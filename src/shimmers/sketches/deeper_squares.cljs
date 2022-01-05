(ns shimmers.sketches.deeper-squares
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; Variations on (Des)Ordres
;; https://dam.org/museum/artists_ui/artists/molnar-vera/des-ordres/

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn scale-rect [rect scale]
  (g/scale-size rect scale))

(defn deepen [{op :p [width height] :size :as outer}]
  (let [{ip :p [w h] :size :as inner} (scale-rect outer (dr/random 0.75 0.95))]
    (g/translate inner (tm/+ (gv/vec2 (* (dr/random 0.2 0.8) (- width w))
                                      (* (dr/random 0.2 0.8) (- height h)))
                             (tm/- op ip)))))

(defn shapes []
  (let [w 100
        h 100
        o 5]
    (for [x (range (/ width w))
          y (range (/ height h))]
      (svg/group {:transform (str "translate(" (* x w) "," (* y h) ")")}
                 (take 12 (iterate deepen (rect/rect o o (- w o) (- h o))))))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 0.5}
            (shapes)))

(sketch/definition deeper-squares
  {:created-at "2022-01-04"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount (view-sketch/page-for scene :deeper-squares)
              "sketch-host"))
