(ns shimmers.sketches.prophecies
  (:require
   [reagent-keybindings.keyboard :as kb]
   [shimmers.common.svg :as csvg]
   [shimmers.common.svg-export :as svg-export]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.collisions :as collide]
   ;; side-effect extend-type to Line2
   [shimmers.math.geometry.line]
   [shimmers.math.hexagon :as hex]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.types :refer [Circle2]]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn right [angle]
  (+ angle tm/HALF_PI))

(defn left [angle]
  (- angle tm/HALF_PI))

(defn deepen [polygon n]
  (let [scale (/ 0.8 (inc n))]
    (->> polygon
         (iterate (fn [poly]
                    (g/scale-size poly (- 1.0 scale))))
         (take (inc n)))))

(defn nested [polygon n]
  (->> (range 0.0 0.8 (/ 0.8 n))
       (mapv (fn [s] (g/scale-size polygon (- 1.0 s))))))

(defn maybe [prob operation]
  (if (dr/chance prob)
    (operation)
    []))

(defn square [connect size angle]
  (-> (rect/rect size)
      (g/center)
      (g/rotate angle)
      (g/translate (tm/+ connect (v/polar (* 0.5 size) angle)))))

(defn circle [connect size angle]
  (-> connect
      (v/+polar (* 0.5 size) angle)
      (gc/circle (* 0.5 size))))

(defn point-triangle [connect size angle]
  (gp/polygon2 [connect
                (v/+polar connect size (- angle 0.5))
                (v/+polar connect size (+ angle 0.5))]))

(defn edge-triangle [connect size angle]
  (gp/polygon2 [(v/+polar connect (* 0.5 size) (left angle))
                (v/+polar connect size angle)
                (v/+polar connect (* 0.5 size) (right angle))]))

(defn flat-hex [connect size angle]
  (let [r (* 0.5 size)
        center (v/+polar connect (hex/apothem {:r r}) angle)]
    (-> (gc/circle r)
        (hex/flat-hexagon->polygon)
        (g/rotate (+ angle (/ Math/PI 6)))
        (g/translate center))))

(defn pointy-hex [connect size angle]
  (let [r (* 0.5 size)
        center (v/+polar connect r angle)]
    (-> (gc/circle r)
        (hex/pointy-hexagon->polygon)
        (g/rotate (+ angle (/ Math/PI 6)))
        (g/translate center))))

(defn n-gon [n center radius angle]
  (-> (gc/circle radius)
      (g/as-polygon n)
      (g/rotate angle)
      (g/translate center)))

;; https://en.wikipedia.org/wiki/Pentagon
(defn flat-pentagon [connect size angle]
  (let [size (* 0.66 size)
        r (/ 1 (* 2 (Math/sqrt (- 5 (Math/sqrt 20)))))
        R (Math/sqrt (/ (+ 5 (Math/sqrt 5)) 10))
        center (v/+polar connect (* r size) angle)]
    (n-gon 5 center (* R size) angle)))

(defn pointy-pentagon [connect size angle]
  (let [size (* 0.66 size)
        R (Math/sqrt (/ (+ 5 (Math/sqrt 5)) 10))
        center (v/+polar connect (* R size) angle)
        angle (- angle (* 0.5 (tm/radians 72)))]
    (n-gon 5 center (* R size) angle)))

(defn flat-heptagon [connect size angle]
  (let [R (* 0.66 size)
        apothem (* R (Math/cos (/ Math/PI 7)))
        center (v/+polar connect apothem angle)]
    (n-gon 7 center R angle)))

(defn pointy-heptagon [connect size angle]
  (let [R (* 0.66 size)
        center (v/+polar connect R angle)
        angle (+ angle (tm/radians (+ 128 (/ 4 7))))]
    (n-gon 7 center R angle)))

(defn flat-octagon [connect size angle]
  (let [a (* 0.33 size)
        R (* (/ (Math/sqrt (+ 4 (* 2 (Math/sqrt 2)))) 2) a)
        r (* (/ (+ 1 (Math/sqrt 2)) 2) a)
        center (v/+polar connect r angle)
        angle (+ angle (* 0.5 (tm/radians 135)))]
    (n-gon 8 center R angle)))

(defn pointy-octagon [connect size angle]
  (let [a (* 0.33 size)
        R (* (/ (Math/sqrt (+ 4 (* 2 (Math/sqrt 2)))) 2) a)
        center (v/+polar connect R angle)]
    (n-gon 8 center R angle)))

(defn inner-angle-n-gon [n]
  (let [sum-of-internal (* (- n 2) 180)]
    (/ sum-of-internal n)))

;; Why does this work for 6,7,8 but not 5?
(comment (inner-angle-n-gon 3))

(def poly-shapes
  {:square square
   :circle circle
   :point point-triangle
   :edge edge-triangle
   :flat-pentagon flat-pentagon
   :pointy-pentagon pointy-pentagon
   :flat-hex flat-hex
   :pointy-hex pointy-hex
   :flat-heptagon flat-heptagon
   :pointy-heptagon pointy-heptagon
   :flat-octagon flat-octagon
   :pointy-octagon pointy-octagon
   })

(defn point-on-segment? [point p q]
  (< (g/dist-squared point (gu/closest-point-on-segment point p q)) 1))

(defn face-connectors [connect shape scale]
  (zipmap (if (instance? Circle2 shape)
            []
            (->> (g/edges shape)
                 (remove (fn [[p q]] (point-on-segment? connect p q)))
                 (map (fn [[p q]]
                        (let [midpoint (tm/mix p q 0.5)
                              angle (left (g/heading (tm/- q p)))]
                          {:vertex midpoint
                           :direction angle
                           :scale (* (dr/weighted {1 8
                                                   1.1 4
                                                   1.25 2
                                                   1.5 1
                                                   2.0 1})
                                     scale)})))))
          (repeatedly #(* (dr/weighted {6 6
                                        8 3
                                        12 1
                                        24 1})
                          scale))))

(defn meridian [c1 c2]
  (let [dir (tm/normalize (tm/- (:p c2) (:p c1)))]
    (gl/line2 (tm/- (:p c1) (tm/* dir (:r c1)))
              (tm/+ (:p c2) (tm/* dir (:r c2))))))

(defn gen-direction [heading]
  ((dr/rand-nth [right left]) heading))

(defn gen-size []
  (* (dr/weighted {0.15 1
                   0.1  2
                   0.05 4})
     width))

(defn make-shape [connector shapes]
  (let [{:keys [vertex scale] angle :direction} connector
        len (* scale (gen-size))
        size (* scale (gen-size))
        connect (v/+polar vertex len angle)
        shape ((dr/rand-nth (vals poly-shapes)) connect size angle)
        line (gl/line2 vertex connect)
        padded (g/scale-size shape 1.2)]
    (when (and (collide/bounded? (rect/rect width height) padded)
               (not-any? (fn [s] (collide/overlaps? s padded)) shapes)
               (not-any? (fn [s] (collide/overlaps? s line)) shapes))
      (concat [line shape]
              (maybe 0.5 (partial (dr/rand-nth [deepen nested]) shape
                                  (dr/random-int 3 8)))))))

(defn add-shapes [shapes connectors n]
  (loop [n n connectors connectors shapes shapes attempts (* n 5)]
    (if (or (zero? n) (zero? attempts) (empty? connectors))
      [shapes connectors]
      (let [{:keys [scale] :as connector} (dr/weighted connectors)]
        (if-let [new-shapes (make-shape connector shapes)]
          (let [{[_ connect ] :points} (first new-shapes)
                faces (face-connectors connect (second new-shapes) (* 0.66 scale))]
            (recur (dec n)
                   (merge (dissoc connectors connector) faces)
                   (concat shapes new-shapes)
                   (dec attempts)))
          (recur n connectors shapes (dec attempts)))))))

(defn gen-connectors [meridian n heading]
  (for [vertex (->> (g/vertices meridian n)
                    (drop-last 1)
                    (drop 1))
        direction [left right]]
    {:vertex vertex
     :direction (direction heading)
     :scale 1.0}))

(defn shapes []
  (let [cut (dr/rand-nth [(/ 1 3) (/ 1 4) (/ 2 5)])
        slant
        (if (dr/chance 0.80) 0.0
            (* (dr/rand-nth [-1 1])
               (dr/weighted {0.20 1
                             0.15 1
                             0.1 3
                             0.05 2})))
        c1-p (rv (dr/random 0.35 0.35) 0.5)
        c2-p (rv (dr/random 0.7 0.75) (+ 0.5 slant))
        d (g/dist c1-p c2-p)
        c1 (gc/circle c1-p (* d (- 1 cut)))
        c2 (gc/circle c2-p (* d cut))
        meridian (meridian c1 c2)
        skew (if (dr/chance 0.75) 0.0
                 (dr/rand-nth [(* (dr/rand-nth [1 -1]) (dr/rand-nth [(/ Math/PI 16) (/ Math/PI 9)]))
                               (- (g/heading meridian))]))
        heading (+ (g/heading meridian) skew)

        n-shapes
        (dr/weighted
         {(dr/random-int 8 16) 12
          (dr/random-int 12 20) 4
          (dr/random-int 16 30) 1})

        connectors
        (zipmap (gen-connectors meridian (dr/random-int 8 24) heading)
                (repeat 5))

        [shapes _]
        (add-shapes [] connectors n-shapes)]
    (concat [c1 c2 meridian]
            shapes)))

(defonce ui-state (ctrl/state {:filled true}))

(defn scene []
  (let [shapes (shapes)]
    (fn []
      (csvg/svg {:id "scene"
                 :width width
                 :height height
                 :stroke "black"
                 :fill (if (:filled @ui-state) "white" "none")
                 :stroke-width 1.0}
        shapes))))


(defn ui-controls []
  [:div
   [kb/kb-action "alt-s" #(svg-export/download "scene" "prophecies")]
   [ctrl/checkbox ui-state "Filled" [:filled]]])

(sketch/definition prophecies
  {:created-at "2022-07-08"
   :type :svg
   :tags #{:deterministic}}
  (-> scene
      (view-sketch/page-for :prophecies ui-controls)
      (ctrl/mount "sketch-host")))
