(ns shimmers.sketches.circuit-diagrams
  (:require
   [clojure.set :as set]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.arc :as arc]
   [shimmers.math.geometry.collisions :as collide]
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

(defonce ui-state
  (ctrl/state {:annotation true
               :circuits true
               :shapes true}))

(defonce defo (debug/state))

(defn random-shape [size]
  (let [n (dr/weighted {3 8 4 4 5 4 6 2})
        r (poly/circumradius-side-length n size)]
    (if (= n 5)
      (poly/pentagon (poly/circumradius-side-length 6 size)
                     (dr/random-int 6))
      (poly/regular-n-gon n r))))

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

(defn tiles-structure? [structure shape]
  (let [overlaps
        (filter (fn [s] (and (collide/overlaps? s shape)
                            (not (collide/bounded? s shape))))
                structure)
        cross
        (for [edge (g/edges shape)
              s-edge (mapcat g/edges overlaps)
              :when (collide/cross-edge? edge s-edge)]
          [edge s-edge])
        coincident
        (for [edge (g/edges shape)
              s-edge (mapcat g/edges overlaps)
              :when (collide/coincident-edge edge s-edge)]
          [edge s-edge])]
    (or (empty? overlaps)
        (and (empty? cross)
             (empty? coincident)))))

(defn opposing-face-apothem [shape heading]
  (when-let [opposing-face
             (some (fn [face]
                     (let [[p q] face
                           angle (g/heading (g/normal (tm/- q p)))]
                       (when (tm/delta= angle heading 0.01)
                         face)))
                   (g/edges shape))]
    (let [[a b] opposing-face]
      (tm/mix a b 0.5))))

(defn tiling [{:keys [size bounds]} seed n]
  (loop [structure [seed]
         faces (set (g/edges seed))
         attempts 512]
    (if (or (empty? faces) (>= (count structure) n) (zero? attempts))
      structure
      (let [face (dr/rand-nth (into [] faces))
            [fp fq] face
            mid (tm/mix fp fq 0.5)
            structure-face (g/normal (tm/- fq fp))
            angle (g/heading (tm/- structure-face))
            shape (g/rotate (g/center (random-shape size)) angle)
            apothem (or (opposing-face-apothem shape angle)
                        (->> (poly/apothem-side-length (count (g/vertices shape)) size)
                             (tm/normalize structure-face)))
            pos (tm/- mid apothem)
            shape' (g/center shape pos)
            edges (drop 1 (sort-by (fn [[p q]] (g/dist-squared mid (tm/mix p q 0.5)))
                                   (g/edges shape')))
            inside? (collide/bounded? bounds shape')
            tiles? (tiles-structure? structure shape')
            notes [(gc/circle mid 3.0)
                   (gc/circle pos 5.0)
                   (gl/line2 pos
                             (tm/+ pos (or (opposing-face-apothem shape angle)
                                           (gv/vec2 5 5))))
                   (gl/line2 fp fq)]]
        (if (and inside? tiles?)
          (recur (conj structure
                       (vary-meta shape' assoc
                                  :annotation notes
                                  :parent {:face face}))
                 (set/union (disj faces face) (set edges))
                 attempts)
          (recur structure
                 (if (< (:attempt (meta face)) 4)
                   (conj (disj faces face) (vary-meta face update :attempt (fnil inc 0)))
                   (disj faces face))
                 (dec attempts))
          )))))

(defn draw-face [{:keys [connected] :as face}]
  [(vary-meta (gc/circle (face-center face) 6)
              assoc :fill (if connected "black" "white"))])

(defn connector [face pt]
  (gl/line2 (face-center face) pt))

(defn bar [pt r angle]
  (gl/line2 (v/+polar pt r (- angle (* 0.25 eq/TAU)))
            (v/+polar pt r (+ angle (* 0.25 eq/TAU)))))

(defn resistor [steps p q]
  (let [a (tm/mix p q 0.2)
        b (tm/mix p q 0.8)
        perp (g/scale (g/rotate (tm/- b a) (* 0.25 eq/TAU)) (/ 1.0 steps))]
    (csvg/group {}
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
    (csvg/group {}
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
    (csvg/group {}
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
             (gc/circle center (* radius (/ 1 3)))])
          (when (seq out)
            (concat [(let [margin (* eq/TAU 0.75 (/ 1.0 (inc (count faces))))
                           t0 (- (face-angle (first out) center) margin)
                           t1 (+ (face-angle (last out) center) margin)]
                       (vary-meta (arc/arc center radius t0 t1)
                                  assoc :fill "none"))]
                    #_[(gc/circle (tm/mix center (face-center (first out)) 0.33) 4.0)
                       (gc/circle (tm/mix center (face-center (last out)) 0.66) 4.0)]
                    (for [face out]
                      (connector face (v/+polar center radius (face-angle face center)))))))))

(defn shape-click [component]
  (fn []
    (reset! defo (assoc component
                        :meta (meta (:shape component))))))

(defn draw-shape [{:keys [id shape faces] :as component}]
  (let [m {:on-click (shape-click component)}]
    (into [(vary-meta (vary-meta shape dissoc :parent) merge
                      (if (= (:id @defo) id)
                        (assoc m :stroke "blue"
                               :stroke-width 1.5)
                        m))]
          (mapcat draw-face faces))))

(defn make-component [idx shape]
  (let [kind (dr/weighted {:wire 3 :orb 1})]
    {:id idx
     :shape (vary-meta shape dissoc :annotation)
     :annotation (:annotation (meta shape))
     :kind kind
     :faces (face-normals shape)}))

(defn shapes []
  (let [size (* 0.15 height)
        center (rv 0.5 0.5)
        seed (random-shape size)
        shape (g/center (g/rotate seed 0) center)
        structure
        (tiling {:size size :bounds (g/scale-size (csvg/screen width height) 0.95)}
                shape 36)]
    (map-indexed make-component structure)))

(defn scene [{:keys [scene-id]}]
  (csvg/timed
   (let [components (shapes)]
     (reset! defo {})
     (fn []
       (csvg/svg {:id scene-id
                  :width width
                  :height height
                  :stroke "black"
                  :fill "white"
                  :stroke-width 0.5
                  ;; required for on-click to fire on pointer events within group/polygon clip path
                  :style {:pointer-events "fill"}}
         (when (:shapes @ui-state)
           (csvg/group {}
             (mapcat draw-shape components)))
         (when (:circuits @ui-state)
           (csvg/group {:stroke-width 1.25}
             (mapcat draw-component components)))
         (when (:annotation @ui-state)
           (csvg/group {:stroke "red" :fill "none" :stroke-width 1.0}
             (mapcat :annotation components))))))))

(defn ui-controls []
  [:div
   [:div
    [ctrl/checkbox ui-state "Annotation" [:annotation]]
    [ctrl/checkbox ui-state "Circuits" [:circuits]]
    [ctrl/checkbox ui-state "Shapes" [:shapes]]]
   [:div
    [:h4 "Debug"]
    (debug/display defo)]])

(sketch/definition circuit-diagrams
  {:created-at "2024-12-10"
   :tags #{}
   :type :svg}
  (ctrl/mount
   (usvg/page (assoc sketch-args
                     :explanation ui-controls)
              scene)))
