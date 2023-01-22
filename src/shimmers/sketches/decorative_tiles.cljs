(ns shimmers.sketches.decorative-tiles
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.sketch :as sketch :include-macros true]
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
   {:recursion-depth 4
    :auto-scale true
    :base-size 40
    :spacing-size 4
    :variable-size true
    :limit-overlap true
    :max-overlap 0}))

;; something is wrong with the facing signs
(defn connections [shape dir]
  (for [[a b] (g/edges shape)
        :let [mid (tm/mix a b 0.5)
              am (tm/- (tm/- mid (g/centroid shape)))]
        :when (or (not dir)
                  (> (sm/radial-distance (g/heading am) (g/heading dir)) 0.1))]
    [shape mid]))

(defn connection-pt [shape dir]
  (some (fn [[a b]]
          (let [mid (tm/mix a b 0.5)
                am (tm/- (tm/- mid (g/centroid shape)))]
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

(defn gen-shape []
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
     (/ 1 3) 1})])

(defn excess-overlap [max-overlap new-shape existing-shapes]
  (let [new-bounds (g/bounds new-shape)]
    (when max-overlap
      (loop [overlaps 0 shapes existing-shapes]
        (when (seq shapes)
          (let [{:keys [shape bounds]} (first shapes)]
            (if (and (isec/intersect-rect-rect? bounds new-bounds)
                     (collide/overlaps? shape new-shape))
              (if (> (inc overlaps) max-overlap)
                true
                (recur (inc overlaps) (rest shapes)))
              (recur overlaps (rest shapes)))))))))

(defn shape-wrapper [s]
  {:shape s
   :bounds (g/bounds s)})

(defn layers [seed plan size max-overlap spacing-size]
  (loop [plan plan layer [seed] shapes []]
    (let [shapes' (into shapes (map shape-wrapper layer))]
      (if (empty? plan)
        shapes'
        (let [connects
              (->> layer
                   (mapcat
                    (fn [s]
                      (let [dir (when-let [parent (:parent s)]
                                  (tm/- (g/centroid s) (g/centroid parent)))]
                        (connections s dir)))))
              [m-shape mult] (first plan)
              scale (if (:variable-size @ui-state)
                      mult
                      1)]
          (recur
           (rest plan)
           (->>
            (for [[shape connect] connects]
              (let [dir (tm/- connect (g/centroid shape))
                    angle (g/heading dir)
                    addition (g/rotate (m-shape (* size scale)) angle)
                    connect-pt (connection-pt addition dir)]
                (-> addition
                    (g/translate (tm/+ connect connect-pt
                                       (tm/normalize connect-pt spacing-size)))
                    (assoc :parent shape))))
            (remove (fn [shape] (excess-overlap max-overlap shape shapes'))))
           shapes'))))))

(defn shapes [plan base-size max-overlap spacing-size]
  (let [size base-size
        [m-shape mult] (first plan)]
    (layers (m-shape (* mult size))
            (rest plan)
            size
            max-overlap
            spacing-size)))

(defn scene [plan auto-scale base-size max-overlap spacing-size]
  (csvg/timed
   (csvg/svg {:width width
              :height height
              :stroke "black"
              :fill-opacity "5%"
              :fill "black"
              :stroke-width 1.0}
     (let [tiles (shapes plan base-size max-overlap spacing-size)
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

(defn page []
  (let [plan (vec (repeatedly 11 gen-shape))]
    (fn []
      (let [{:keys [recursion-depth
                    auto-scale base-size
                    limit-overlap max-overlap
                    spacing-size]}
            @ui-state]
        [:div
         [:div.canvas-frame [scene (take recursion-depth plan)
                             auto-scale
                             base-size
                             (when limit-overlap max-overlap)
                             spacing-size]]
         [:div.contained
          [:div.flexcols
           (ctrl/container
            [:p]
            (ctrl/numeric ui-state "Recursion Depth" [:recursion-depth] [1 9 1])
            (ctrl/checkbox ui-state "Limit Overlap" [:limit-overlap])
            (when limit-overlap
              (ctrl/numeric ui-state "Max Overlap with Prior Layer" [:max-overlap] [0 100 1]))
            (ctrl/checkbox ui-state "Auto Scaling" [:auto-scale])
            (when-not auto-scale
              (ctrl/numeric ui-state "Base Size" [:base-size] [20 100 1]))
            (ctrl/numeric ui-state "Spacing" [:spacing-size] [1 20 1])
            (ctrl/checkbox ui-state "Variable Size" [:variable-size]))
           [:div
            [:p.center (view-sketch/generate :decorative-tiles)]
            [:p.center "Recursively layer regular polygons on each outward face."]]]]]))))

(sketch/definition decorative-tiles
  {:created-at "2023-01-20"
   :type :svg
   :tags #{}}
  (ctrl/mount page "sketch-host"))
