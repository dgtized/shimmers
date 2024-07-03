(ns shimmers.sketches.prophecies
  (:require
   [clojure.math :as math]
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.algorithm.lines :as lines]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.math.geometry.line]
   [shimmers.math.geometry.polygon :as poly]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
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

(defn spiral [polygon n]
  (let [scale (/ 1.1 (inc n))
        dt (/ tm/PHI (inc n))]
    (->> [polygon 0]
         (iterate (fn [[poly t]]
                    [(g/translate (g/scale-size poly (- 1.0 scale))
                                  (tm/* (tm/- (g/point-at poly (mod (+ t 0.5) 1.0))
                                              (g/centroid poly))
                                        (/ 0.3 (inc n))))
                     (+ t dt)]))
         (take (int (* 1.2 (inc n))))
         (map first))))

(defn clipping [theta polygon n]
  (let [{[cx _] :p radius :r :as bounding } (g/bounding-circle polygon)
        [xstart ystart] (rp/inside-circle bounding dr/random)
        cosa (math/cos theta)
        m (math/tan theta)
        c (- ystart (* m xstart))
        x0 (- cx (* 1.2 radius))
        y0 (+ (* m x0) c)
        x1 (+ cx (* 1.2 radius))
        y1 (+ (* m x1) c)
        spacing (/ (max (g/width polygon) (g/height polygon)) (int (* 1.33 (inc n))))]
    (clip/hatching-middle-out (fn [a b]
                                (first (lines/clip-line (gl/line2 a b) polygon)))
                              spacing cosa
                              [x0 y0] [x1 y1])))

(defn flat-polygon [n]
  (fn [connect circumradius angle]
    (let [r (poly/apothem-circumradius n circumradius)]
      (-> (poly/regular-n-gon n circumradius)
          (g/rotate angle)
          (g/translate (v/+polar connect r angle))))))

(defn point-polygon [n]
  (fn [connect circumradius angle]
    (-> (poly/regular-n-gon n circumradius)
        (g/rotate (+ angle (/ eq/TAU (* 2 n))))
        (g/translate (v/+polar connect circumradius angle)))))

(defn circle [connect radius angle]
  (-> connect
      (v/+polar radius angle)
      (gc/circle radius)))

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

(defn stroke-width [s width]
  (vary-meta s assoc :stroke-width width))

(def poly-shapes
  (into [{:sides 20 :shape-fn circle}]
        (mapcat (fn [sides]
                  [{:sides sides :shape-fn (flat-polygon sides)}
                   {:sides sides :shape-fn (point-polygon sides)}])
                (range 3 9))))

(defn inside [s _]
  [(vary-meta (g/scale-size s 0.95)
              assoc :fill "none")])

(defn outside [s _]
  [(vary-meta (g/scale-size s 1.05)
              assoc :fill "none")])

(defn inside-outside [s _]
  [(vary-meta (g/scale-size s 1.05)
              assoc :fill "none")
   (vary-meta (g/scale-size s 0.95)
              assoc :fill "none")])

(defn make-shape [connector shapes]
  (let [{:keys [vertex scale] angle :direction} connector
        len (* scale (gen-size))
        radius (* scale (/ 1 tm/PHI) (gen-size))
        connect (v/+polar vertex len angle)
        {:keys [shape-fn]} (dr/rand-nth poly-shapes)
        shape (shape-fn connect radius angle)
        line (gl/line2 vertex connect)
        padded (g/scale-size shape 1.2)
        p-area (/ (g/area shape) width)
        clipper-r (partial clipping (dr/gaussian (+ angle tm/HALF_PI) 0.33))
        clipper-p (partial clipping (+ angle tm/HALF_PI))
        ;; clipper-a (partial clipping angle) -- vertical lines don't work
        ]
    (when (and (collide/bounded? (rect/rect width height) padded)
               (not-any? (fn [s] (collide/overlaps? s padded)) shapes)
               (not-any? (fn [s] (collide/overlaps? s line)) shapes))
      ;; TODO: add hatching here, but not supporting polygons yet
      (let [shading
            (if (dr/chance 0.5)
              ((dr/weighted [[deepen 4]
                             [nested 4]
                             [spiral 1]
                             [clipper-r 1]
                             [clipper-p 1]
                             [inside 1]
                             [outside 1]
                             [inside-outside 1]])
               shape
               (int (* (if (< p-area 1.2) (- p-area 0.2) 1.0)
                       (dr/random-int 3 9))))
              [])
            shape'
            (if (and (empty? shading) (dr/chance 0.33))
              (stroke-width shape (dr/gaussian 1.5 0.5))
              shape)]
        (concat [line shape'] shading)))))

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

(defn cut-percent []
  (let [cut (dr/rand-nth [(/ 1 3) (/ 1 4) (/ 2 5) (/ 1 5)])]
    (if (dr/chance 0.5) (- 1 cut) cut)))

(defn slant-grade []
  (if (dr/chance 0.50) 0.0
      (* (dr/rand-nth [-1 1])
         (dr/weighted {0.20 1
                       0.15 1
                       0.1 3
                       0.05 2}))))

(defn random-skew [meridian]
  (if (dr/chance 0.75) 0.0
      (dr/rand-nth [(* (dr/rand-nth [1 -1])
                       (dr/rand-nth [(/ math/PI 16) (/ math/PI 9)]))
                    (- (g/heading meridian))])))

;; Suppose a rectangle of width W, with a horizontal line at center. Two circles
;; with a particular size relation `cut` need to fit on that center line such
;; that neither extend beyond the bounds of the rectangle. If ignoring the top
;; and bottom bounds, then one circle has diameter `cut` and the other `1-cut`.
;; As example, a `cut` of 1/4 means the larger circle should have 3/4 the
;; diameter and the small should have 1/4.
;;
;; If we constrain on height `H` as well, then the center point may need to
;; approach the center, such that the radius of the larger circle `cut/2` is
;; less than 1/2 the height. In this case the center points would need to be
;; spaced closer to ensure radius is reduced to adjust for height.
;;
;; Let `d` be the distance between the two circle centers. Radius of large
;; circle is `1-cut/2`, radius of small circle is `cut/2`. The maximum radius of
;; large circle is 1/2 height, so it's x-coordinate must be at last `(1-cut)/2`.
;;
(defn scale-fit [bounds l r cut]
  (loop [{[p q] :points :as line} (gl/line2 l r)]
    (let [d (g/dist p q)
          dl (poly/dist-to-closest-point bounds p)
          dr (poly/dist-to-closest-point bounds q)
          r (max (* d cut) (* d (- 1 cut)))
          max-r (max dl dr)]
      (cond (< r (* 0.975 max-r))
            (recur (g/scale-size line (dr/random 1.00 1.1)))
            (> r max-r)
            (recur (g/scale-size line (dr/random 0.9 1.00)))
            :else
            (let [x (* 0.5
                       (if (> (- 1 cut) cut)
                         (- (g/width bounds) (* cut d) (:x q))
                         (- (- (:x p) (* (- 1 cut) d)))))]
              (g/translate line (gv/vec2 x (* x (g/slope-xy line)))))))))

(defonce ui-state (ctrl/state {:filled true}))

(defn shapes []
  (let [bounds (csvg/screen width height)
        cut (cut-percent)
        slant (slant-grade)
        y-pos (+ 0.5 (/ slant 2))
        {[left-p right-p] :points}
        (scale-fit bounds
                   (rv 0.4 (- 1 y-pos))
                   (rv 0.6 y-pos)
                   cut)
        d (g/dist left-p right-p)
        c-left (gc/circle left-p (* d (- 1 cut)))
        c-right (gc/circle right-p (* d cut))
        meridian (meridian c-left c-right)
        skew (random-skew meridian)
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
    {:shapes (concat [c-left c-right meridian] shapes)
     :debug [(gc/circle left-p 3.0)
             (gc/circle right-p 3.0)]}))

(defn scene [{:keys [scene-id]}]
  (csvg/timed
   (let [{:keys [shapes debug]} (shapes)]
     (fn []
       (let [{:keys [filled]} @ui-state]
         (csvg/svg {:id scene-id
                    :width width
                    :height height
                    :stroke "black"
                    :fill (if filled "white" "none")
                    :stroke-width 1.0}
           (if filled
             shapes
             (concat shapes debug))))))))


(defn ui-controls []
  [:div
   [ctrl/checkbox ui-state "Filled" [:filled]]])

(sketch/definition prophecies
  {:created-at "2022-07-08"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount (usvg/page (assoc sketch-args :explanation ui-controls) scene)))
