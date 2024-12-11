(ns shimmers.sketches.circuit-diagrams
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.arc :as arc]
   [shimmers.math.geometry.polygon :as poly]
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

(defn face-normals [polygon]
  (->>
   polygon
   g/edges
   (map-indexed
    (fn [i [p q]]
      {:id i
       :edge [p q]
       :connected (dr/weighted {false 3 true 2})}))))

(defn face-center [{[p q] :edge}]
  (tm/mix p q 0.5))

(defn draw-face [{:keys [connected] :as face}]
  [(vary-meta (gc/circle (face-center face) 6)
              assoc :fill (if connected "black" "white"))])

(defn connector [face pt]
  (vary-meta (gl/line2 (face-center face) pt)
             assoc :stroke-width 1.25))

(defn draw [{:keys [shape kind faces]}]
  (concat [shape]
          (case kind
            :wire
            (let [center (g/centroid shape)]
              (keep (fn [{conn :connected :as face}]
                      (when conn
                        (connector face center))) faces))
            :orb
            (let [center (g/centroid shape)
                  [in & out] (filter :connected faces)]
              (into (when in
                      [(connector in center)
                       (gc/circle center 4.0)])
                    (when (seq out)
                      (into [(arc/arc center 10.0
                                      (- (g/heading (tm/- (face-center (first out)) center)) 0.5)
                                      (+ (g/heading (tm/- (face-center (last out)) center)) 0.5))]
                            (for [face out]
                              (connector face (tm/mix center (face-center face) 0.1))))))))
          (mapcat draw-face faces)))

(defn make-component [shape]
  {:shape shape
   :kind (dr/weighted {:wire 1 :orb 1})
   :faces (face-normals shape)})

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
