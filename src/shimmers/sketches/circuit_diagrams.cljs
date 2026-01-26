(ns shimmers.sketches.circuit-diagrams
  (:require
   [clojure.set :as set]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.geometry.arc :as arc]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.math.geometry.polygon :as poly]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [thi.ng.strf.core :as f]))

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

(defn s-midpoint [[p q]]
  (let [[x y] (tm/mix p q 0.5)]
    (f/format [(f/float 2) "," (f/float 2)] x y)))

;; FIXME: ensure at least one pass through connection for resistor/wire/capacitor
;; FIXME: face-normals needs to take shape/kind to avoid adding connection types for orbs
(defn face-normals [polygon neighbors]
  (let [faces (->>
               polygon
               g/edges
               (map-indexed
                (fn [i [p q]]
                  (let [mid (s-midpoint [p q])]
                    {:id i
                     :edge [p q]
                     :connected (or (some? (get neighbors mid))
                                    (dr/chance 0.15))}))))
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

(defn face-normal [[p q]]
  (g/normal (tm/- p q)))

(defn opposing-face-apothem [shape heading]
  (when-let [opposing-face
             (some (fn [face]
                     (let [angle (g/heading (face-normal face))
                           ;; this happens for edges on squares it's rotated off?
                           angle2 (g/heading (face-normal (reverse face)))]
                       (when (or (tm/delta= angle heading 0.1)
                                 (tm/delta= angle2 heading 0.1))
                         face)))
                   (g/edges shape))]
    (let [[a b] opposing-face]
      (tm/- (tm/mix a b 0.5)))))

(defn matching-length? [shape [fp fq]]
  (let [mid (tm/mix fp fq 0.5)]
    (when-let [matching-face
               (some (fn [[p q]] (when (tm/delta= mid (tm/mix p q 0.5) 1.0)
                                  [p q]))
                     (g/edges shape))]
      (tm/delta= (tm/mag-squared (tm/- (second matching-face) (first matching-face)))
                 (tm/mag-squared (tm/- fq fp))
                 1.0))))

(defn rotate-to-face [shape angle]
  (let [edge (dr/rand-nth (g/edges shape))
        edge-angle (g/heading (face-normal (reverse edge)))]
    (g/rotate shape (- angle edge-angle))))

(defn same-face? [edge face]
  (= (s-midpoint edge) (s-midpoint face)))

;; FIXME: face removal is occasionally allowing overlapping shapes
;; might be for triangles in particular, maybe because one face is shared?
(defn tiling [{:keys [size bounds]} seed n]
  (loop [structure [seed]
         faces (set (map (fn [e] (vary-meta e assoc :shape seed))
                         (g/edges seed)))
         attempts 512]
    (if (or (empty? faces) (>= (count structure) n) (zero? attempts))
      structure
      (let [face (dr/rand-nth (into [] faces))
            [fp fq] face
            mid (tm/mix fp fq 0.5)
            structure-face (face-normal face)
            angle (g/heading structure-face)
            shape (rotate-to-face (g/center (random-shape size)) angle)
            apothem (opposing-face-apothem shape angle)
            pos (tm/+ mid apothem)
            shape' (g/translate shape pos)
            inside? (collide/bounded? bounds shape')
            match-edge-length? (matching-length? shape' face)
            tiles? (tiles-structure? structure shape')
            centroid? (filter (fn [s] (collide/bounded? s (g/centroid shape'))) structure)
            edges (remove (fn [edge]
                            (some (fn [face] (when (same-face? edge face)
                                              face))
                                  faces))
                          (map (fn [e] (vary-meta e assoc :shape shape'))
                               (g/edges shape')))
            notes
            (concat
             [(gc/circle mid 3.0)
              (gc/circle pos 5.0)
              (gl/line2 mid pos)
              #_(gl/line2 fp fq)
              (gl/line2 mid (tm/+ mid (tm/normalize structure-face 8)))
              ]
             (if (g/contains-point? (:shape (meta face)) pos)
               [(gc/circle pos 2.5)]
               [])
             (for [edge (g/edges shape')]
               (let [[p q] edge
                     mid (tm/mix p q 0.5)
                     normal (tm/normalize (face-normal edge) 5)]
                 (gl/line2 mid (tm/+ mid normal))))
             (if (seq centroid?)
               (into [(gc/circle pos 12.0)]
                     (for [s centroid?]
                       (poly-detect/inset-polygon s 8)))
               []))]
        (if (and inside? tiles? match-edge-length?)
          (recur (conj structure
                       (vary-meta shape' assoc
                                  :annotation notes
                                  :parent {:face face}))
                 (set/union (disj faces face) (set edges))
                 attempts)
          (recur structure
                 (if (< (:attempt (meta face)) 12)
                   (conj (disj faces face) (vary-meta face update :attempt (fnil inc 0)))
                   (disj faces face))
                 (dec attempts))
          )))))

(defn draw-face [{:keys [connected] :as face}]
  (if connected
    [(vary-meta (gc/circle (face-center face) 2)
                assoc :fill "black")]
    []))

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
        size (geometry/min-axis shape)
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
        radius (* 0.12 (geometry/min-axis shape))
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
                      (cond (= (:id @defo) id)
                            (assoc m :stroke "blue"
                                   :stroke-width 1.5)
                            (contains? (set (flatten (vals (:neighbors @defo)))) id)
                            (assoc m :stroke "green"
                                   :stroke-width 1.5)
                            :else
                            m))]
          (mapcat draw-face faces))))

(defn make-component [idx shape]
  (let [kind (dr/weighted {:wire 3 :orb 1})]
    {:id idx
     :shape (vary-meta shape dissoc :annotation)
     :annotation (:annotation (meta shape))
     :kind kind}))

(defn midpoints [shape]
  (map s-midpoint (g/edges shape)))

(defn index-midpoint-to-shape-id [components]
  (reduce (fn [idx {:keys [id shape]}]
            (reduce (fn [idx mid] (update idx mid (fnil conj []) id))
                    idx
                    (midpoints shape)))
          {}
          components))

(defn connect-faces [components]
  (let [midpoint-index (index-midpoint-to-shape-id components)]
    (for [{:keys [id shape] :as component} components]
      (let [neighbors
            (reduce (fn [neighbors mid]
                      (let [n (remove #{id} (get midpoint-index mid))]
                        (if (empty? n)
                          neighbors
                          (assoc neighbors mid n))))
                    {} (midpoints shape))]
        (assoc component
               :neighbors neighbors
               :faces (face-normals shape neighbors))))))

(defn gen-components []
  (let [size (* 0.1 height)
        center (rv 0.5 0.5)
        seed (random-shape size)
        shape (g/center (g/rotate seed 0) center)
        structure
        (tiling {:size size :bounds (g/scale-size (csvg/screen width height) 0.95)}
                shape 32)]
    (connect-faces (map-indexed make-component structure))))

(defn scene [{:keys [scene-id]}]
  (csvg/timed
   (let [components (gen-components)]
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
         (for [component components]
           (csvg/group {}
             (when (:shapes @ui-state)
               (csvg/group {:stroke-width 0.33}
                 (draw-shape component)))
             (when (:circuits @ui-state)
               (csvg/group {:stroke-width 1.0}
                 (draw-component component)))
             (when (:annotation @ui-state)
               (csvg/group {:stroke-width 1.5 :stroke "red" :fill "none"}
                 (:annotation component))))))))))

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
   (usvg/page sketch-args ui-controls scene)))

;; alternative approach:
;; Beyond Wave Function Collapse: Procedular Modeling without Tiles
;; https://www.youtube.com/watch?v=1tgMl92DAqk
