(ns shimmers.sketches.circuit-diagrams
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.arc :as arc]
   [shimmers.math.geometry.polygon :as poly]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; Concept: tile the plain with regular-n-gons that don't overlap like in
;; regular-tilings then for the tiles with edges connecting to neighboring
;; n-gons, calculate which faces are connectives and use that to inform the
;; random shape generated inside.

(defn random-shape [size]
  (let [n (dr/weighted {3 8 4 4 5 1 6 2 7 1 8 1})
        r (poly/circumradius-side-length n size)]
    (poly/regular-n-gon n r)))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

;; FIXME: ensure at least one pass through connection for resistor/wire/capacitor
;; FIXME: face-normals needs to take shape/kind to avoid adding connection types for orbs
(defn face-normals [polygon]
  (let [faces (->>
               polygon
               g/edges
               (map-indexed
                (fn [i [p q]]
                  {:id i
                   :edge [p q]
                   :connected (dr/weighted {false 3 true 2})})))
        distribution (case (count (filter :connected faces))
                       0 {}
                       1 {:ground 1}
                       2 (if (dr/chance 0.25)
                           {:ground 1}
                           {:wire 3 :resistor 2 :capacitor 2})
                       {:wire 3 :ground 1 :resistor 2 :capacitor 2})]
    (for [face faces]
      (if (:connected face)
        (let [kind (dr/weighted distribution)]
          (merge (assoc face :kind kind)
                 (case kind
                   :resistor
                   {:params {:resistor-steps (dr/random-int 6 11)}}
                   {})))
        face))))

(defn face-center [{[p q] :edge}]
  (tm/mix p q 0.5))

(defn face-angle [face center]
  (g/heading (tm/- (face-center face) center)))

(defn draw-face [{:keys [connected] :as face}]
  [(vary-meta (gc/circle (face-center face) 6)
              assoc :fill (if connected "black" "white"))])

(defn connector [face pt]
  (vary-meta (gl/line2 (face-center face) pt)
             assoc :stroke-width 2.0))

(defn bar [pt r angle]
  (vary-meta (gl/line2 (v/+polar pt r (- angle (* 0.25 eq/TAU)))
                       (v/+polar pt r (+ angle (* 0.25 eq/TAU))))
             assoc :stroke-width 2.0))

(defn resistor [steps p q]
  (let [a (tm/mix p q 0.2)
        b (tm/mix p q 0.8)
        perp (g/scale (g/rotate (tm/- b a) (* 0.25 eq/TAU)) (/ 1.0 steps))]
    (csvg/group {:stroke-width 2.0}
      (->> (for [n (range (inc steps))]
             (cond (zero? n) a
                   (= steps n) b
                   (odd? n) (tm/+ (tm/mix a b (* n (/ 1.0 steps))) perp)
                   (even? n) (tm/- (tm/mix a b (* n (/ 1.0 steps))) perp)))
           (partition 2 1)
           (map gl/line2)
           (concat
            [(gl/line2 p a)
             (gl/line2 b q)])))))

(defn capacitor [p q]
  (let [a (tm/mix p q 0.45)
        b (tm/mix p q 0.55)
        perp (g/rotate (tm/- b a) (* 0.25 eq/TAU))]
    (csvg/group {:stroke-width 2.0}
      (gl/line2 p a)
      (gl/line2 b q)
      (gl/line2 a (tm/+ a perp))
      (gl/line2 a (tm/- a perp))
      (gl/line2 b (tm/+ b perp))
      (gl/line2 b (tm/- b perp)))))

(defmulti draw-connection :kind)

(defmethod draw-connection :wire
  [face shape]
  (connector face (g/centroid shape)))

(defmethod draw-connection :ground
  [face shape]
  (let [center (g/centroid shape)
        size (min (g/width shape) (g/height shape))
        angle (face-angle face center)
        pt (v/+polar center (* 0.17 size) angle)
        pt2 (v/+polar center (* 0.14 size) angle)
        pt3 (v/+polar center (* 0.11 size) angle)]
    (csvg/group {:stroke-width 2.0}
      (connector face pt)
      (bar pt (* 0.06 size) angle)
      (bar pt2 (* 0.04 size) angle)
      (bar pt3 (* 0.02 size) angle))))

(defmethod draw-connection :resistor
  [{:keys [params] :as face} shape]
  (let [center (g/centroid shape)]
    (resistor (:resistor-steps params)
              (face-center face) center)))

(defmethod draw-connection :capacitor
  [face shape]
  (capacitor (face-center face) (g/centroid shape)))

(defmulti draw-component :kind)

(defmethod draw-component :wire
  [{:keys [shape faces]}]
  (keep (fn [{conn :connected :as face}]
          (when conn
            (draw-connection face shape)))
        faces))

(defmethod draw-component :orb
  [{:keys [shape faces]}]
  (let [center (g/centroid shape)
        radius (* 0.12 (min (g/width shape) (g/height shape)))
        [in & out] (filter :connected faces)]
    (into (when in
            [(connector in center)
             (vary-meta (gc/circle center (* radius (/ 1 3)))
                        assoc :stroke-width 2.0)])
          (when (seq out)
            (concat [(let [margin (* eq/TAU 0.75 (/ 1.0 (inc (count faces))))
                           t0 (- (face-angle (first out) center) margin)
                           t1 (+ (face-angle (last out) center) margin)]
                       (vary-meta (arc/arc center radius t0 t1)
                                  assoc :stroke-width 2.0
                                  :fill "none"))]
                    #_[(gc/circle (tm/mix center (face-center (first out)) 0.33) 4.0)
                       (gc/circle (tm/mix center (face-center (last out)) 0.66) 4.0)]
                    (for [face out]
                      (connector face (v/+polar center radius (face-angle face center)))))))))

(defn draw [{:keys [shape faces] :as component}]
  (concat [shape]
          (mapcat draw-face faces)
          (draw-component component)))

(defn make-component [shape]
  (let [kind (dr/weighted {:wire 3 :orb 1})]
    {:shape shape
     :kind kind
     :faces (face-normals shape)}))

(defn shapes []
  (let [size (* 0.33 height)
        center (rv 0.5 0.5)
        shape (g/center (g/rotate (random-shape size) (dr/random-tau)) center)]
    (mapcat draw [(make-component shape)])))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.5}
    (shapes)))

(sketch/definition circuit-diagrams
  {:created-at "2024-12-10"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
