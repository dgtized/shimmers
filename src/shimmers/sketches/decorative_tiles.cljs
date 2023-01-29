(ns shimmers.sketches.decorative-tiles
  (:require
   [shimmers.common.palette :as palette]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.sketches.radial-mosaic :as radial-mosaic]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.utils.intersect :as isec]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defonce ui-state
  (ctrl/state
   {:recursion-depth 10
    :auto-scale true
    :base-size 40
    :spacing-size 4
    :variable-size true
    :limit-overlap true
    :max-overlap 0
    :color-tiles true
    :single-layer-color false}))

;; FIXME: sort connections so that they are ordered clockwise, but starting from
;; the parent direction instead of order from original shape.
(defn connections [shape dir]
  (for [[a b] (g/edges shape)
        :let [mid (tm/mix a b 0.5)
              am (tm/- (g/centroid shape) mid)]
        :when (or (not dir)
                  (> (sm/radial-distance (g/heading am) (g/heading dir)) 0.1))]
    mid))

(defn connection-pt [shape dir]
  (some (fn [[a b]]
          (let [mid (tm/mix a b 0.5)
                am (tm/- (g/centroid shape) mid)]
            (when (< (sm/radial-distance (g/heading am) (g/heading dir)) 0.1)
              am)))
        (g/edges shape)))

(defn m-square [size]
  (g/center (rect/rect size)))

(defn m-rectangle [angle size]
  (g/rotate (g/center (rect/rect 0 0 size (* tm/PHI size)))
            angle))

;; https://en.wikipedia.org/wiki/Regular_polygon#Circumradius
(defn n-gon
  "Construct a regular polygon with n faces from a circle, and rotate to a flat
  edge at angle zero."
  [n]
  (fn [size]
    (let [s (-> (gc/circle (/ size (* 2 (Math/sin (/ Math/PI n)))))
                (g/as-polygon n))]
      (if (even? n)
        (g/rotate s (/ Math/PI n))
        s))))

(defn gen-shape [palette]
  (fn []
    [(dr/weighted
      [[(n-gon 3) 4]
       [m-square 4]
       [(partial m-rectangle 0) 3]
       [(partial m-rectangle tm/HALF_PI) 2]
       [(n-gon 5) 2]
       [(n-gon 6) 1]
       [(n-gon 7) 1]
       [(n-gon 8) 1]])
     (dr/weighted
      {1 5
       (/ 1 tm/PHI) 2
       0.5 2
       (/ 1 3) 1})
     (vec (repeatedly 10 #(dr/rand-nth palette)))]))

(defn excess-overlap [max-overlap new-shape existing-shapes]
  (let [new-bounds (g/bounds new-shape)]
    (loop [overlaps 0 shapes existing-shapes]
      (when (seq shapes)
        (let [{:keys [shape bounds]} (first shapes)]
          (if (and (isec/intersect-rect-rect? bounds new-bounds)
                   (collide/overlaps? shape new-shape))
            (if (> (inc overlaps) max-overlap)
              true
              (recur (inc overlaps) (rest shapes)))
            (recur overlaps (rest shapes))))))))

(defn shape-wrapper [s]
  {:shape s
   :bounds (g/bounds s)})

(defn mirror [n i]
  (let [mid (Math/floor (/ n 2))]
    (if (even? n)
      (cond (< i mid) (- (dec mid) i)
            :else (- i mid))
      (if (< i mid) (- mid i) (- i mid)))))

(comment (map (partial mirror 7) (range 7))
         (map (partial mirror 8) (range 8)))

(defn extend-shape
  [{:keys [base-size spacing-size color-tiles single-layer-color]}
   [m-shape mult palette]
   shape]
  (let [scale (if (:variable-size @ui-state)
                mult
                1)
        parent-dir (when-let [parent (:parent shape)]
                     (tm/- (g/centroid shape) (g/centroid parent)))
        connects (connections shape parent-dir)]
    (for [[i connect] (map-indexed vector connects)]
      (let [dir (tm/- connect (g/centroid shape))
            angle (g/heading dir)
            addition (g/rotate (m-shape (* base-size scale)) angle)
            connect-pt (connection-pt addition dir)
            ;; TODO figure out how to cycle mover more then one
            ;; color per layer and maintain symmetry needs to be
            ;; symmetric for outbound connections from parent and
            ;; then replicated across each shape in that layer?
            color (if color-tiles
                    (if (or (not parent-dir) single-layer-color)
                      (first palette)
                      (nth palette (mirror (count connects) i)))
                    "black")]
        (-> addition
            (g/translate (tm/+ connect connect-pt
                               (tm/normalize connect-pt (or spacing-size 4))))
            (assoc :parent shape)
            (vary-meta assoc :fill color))))))

(defn layers
  [seed plan {:keys [limit-overlap max-overlap] :as settings}]
  (loop [plan plan layer [seed] shapes []]
    (let [shapes' (into shapes (map shape-wrapper layer))]
      (if (empty? plan)
        shapes'
        (recur (rest plan)
               (->> (mapcat (partial extend-shape settings (first plan)) layer)
                    (remove (fn [shape]
                              (when limit-overlap
                                (excess-overlap max-overlap shape shapes')))))
               shapes')))))

(defn shapes [plan {:keys [base-size] :as settings}]
  (let [[m-shape mult] (first plan)]
    (layers (m-shape (* mult base-size))
            (rest plan)
            settings)))

(defn scene [plan {:keys [auto-scale color-tiles max-overlap] :as settings}]
  (csvg/timed
   (csvg/svg {:width width
              :height height
              :stroke "black"
              :fill-opacity (if color-tiles
                              (/ 2 (+ (or max-overlap 4) 3))
                              "5%")
              :fill "black"
              :stroke-width 1.0}
     (let [tiles (shapes plan settings)
           radial-height (max (* 0.25 height)
                              (apply max (map (comp rect/top :bounds) tiles)))
           scale (/ height (* 2 (+ radial-height 2)))]
       (csvg/group {:transform
                    (str (csvg/translate (rv 0.5 0.5))
                         " "
                         (if auto-scale
                           (csvg/scale scale scale)
                           ""))}
         (map :shape tiles))))))

(defn sanitize-depth [{:keys [limit-overlap recursion-depth]}]
  (let [depth (if (number? recursion-depth)
                recursion-depth
                8)]
    (min (if limit-overlap depth 9)
         depth)))

(defn page []
  (let [palette (dr/rand-nth radial-mosaic/palettes)
        plan (vec (repeatedly 11 (gen-shape palette)))]
    (fn []
      (let [settings @ui-state
            {:keys [auto-scale limit-overlap color-tiles]} settings
            depth (sanitize-depth settings)]
        [:div
         [:div.canvas-frame [scene (take depth plan) settings]]
         [:div.contained
          [:div.flexcols
           (ctrl/container
            [:p]
            (ctrl/numeric ui-state "Recursion Depth" [:recursion-depth] [1 16 1])
            (ctrl/checkbox ui-state "Limit Overlap" [:limit-overlap])
            (when limit-overlap
              (ctrl/numeric ui-state "Max Overlap with Prior Layer" [:max-overlap] [0 100 1]))
            (ctrl/checkbox ui-state "Auto Scaling" [:auto-scale])
            (when-not auto-scale
              (ctrl/numeric ui-state "Base Size" [:base-size] [20 100 1]))
            (ctrl/numeric ui-state "Spacing" [:spacing-size] [1 20 1])
            (ctrl/checkbox ui-state "Variable Size" [:variable-size])
            (ctrl/checkbox ui-state "Color Tiles" [:color-tiles])
            (ctrl/checkbox ui-state "Single Layer Color" [:single-layer-color]))
           [:div
            [:p.center (view-sketch/generate :decorative-tiles)]
            [:p.center "Recursively layer regular polygons on each outward face."]
            (when color-tiles
              [palette/as-svg {:class "center" :width (* 40 (count palette)) :height 30}
               palette])]]]]))))

(sketch/definition decorative-tiles
  {:created-at "2023-01-20"
   :type :svg
   :tags #{}}
  (ctrl/mount page "sketch-host"))
