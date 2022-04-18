(ns shimmers.sketches.deeper-squares
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
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

(defn deepen [{op :p [width height] :size :as outer}]
  (let [{ip :p [w h] :size :as inner} (g/scale-size outer (dr/random 0.75 0.85))
        placement (tm/+ (gv/vec2 (* (dr/random 0.2 0.8) (- width w))
                                 (* (dr/random 0.2 0.8) (- height h)))
                        (tm/- op ip))]
    (g/translate inner placement)))

(defn rotations [group]
  (for [square group]
    (geometry/rotate-around-centroid square (dr/random -0.04 0.04))))

(defn shapes [fx size margin repetition]
  (let [w size
        h size
        edge-margin-w (/ (mod width w) 2)
        edge-margin-h (/ (mod height h) 2)
        o (/ margin 2)
        depth-scale (/ size 72)
        min-depth (Math/ceil (* 4 depth-scale))
        max-depth (Math/ceil (+ min-depth (* 8 depth-scale)))]
    (for [x (range (int (/ width w)))
          y (range (int (/ height h)))
          :let [depth (/ (mod (fx x y) repetition) repetition)]]
      (svg/group {:transform (str "translate(" (+ edge-margin-w (* x w)) "," (+ edge-margin-h (* y h)) ")")}
                 (rotations (take (tm/map-interval depth [0 1] [min-depth max-depth])
                                  (iterate deepen (rect/rect o o (- w o) (- h o)))))))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "none"
             :stroke-width 0.8}
            (let [fx (dr/weighted {- 1
                                   + 1
                                   * 1})
                  rules (dr/weighted {[fx 30 6 8] 1
                                      [fx 72 8 8] 2
                                      [fx 30 8 12] 1
                                      [fx 30 8 16] 1})]
              (apply shapes rules))))

(defn ui-controls []
  [:div
   [:p "Variations on Vera Molnár's "
    [:a {:href "https://dam.org/museum/artists_ui/artists/molnar-vera/des-ordres/"}
     "(Des)Ordres"]]])

(sketch/definition deeper-squares
  {:created-at "2022-01-04"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount (view-sketch/page-for scene :deeper-squares ui-controls)
              "sketch-host"))
