(ns shimmers.sketches.decorative-tiles
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defonce ui-state
  (ctrl/state
   {:recursion-depth 4}))

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
     0.5 1
     (/ 1 tm/PHI) 1})])

(defn layers [seed plan size]
  (loop [plan plan layer [seed] shapes []]
    (if (empty? plan)
      (into shapes layer)
      (let [connects
            (->> layer
                 (mapcat
                  (fn [s]
                    (let [dir (when-let [parent (:parent s)]
                                (tm/- (g/centroid s) (g/centroid parent)))]
                      (connections s dir)))))
            [m-shape mult] (first plan)]
        (recur
         (rest plan)
         (for [[shape connect] connects]
           (let [dir (tm/- connect (g/centroid shape))
                 angle (g/heading dir)
                 addition (g/rotate (m-shape (* size mult)) angle)
                 connect-pt (connection-pt addition dir)]
             (-> addition
                 (g/translate (tm/+ connect (tm/* connect-pt 1.1)))
                 (assoc :parent shape))))
         (into shapes layer))))))

(defn shapes [plan]
  (let [size 40
        [m-shape mult] (first plan)]
    (layers (g/translate (m-shape (* mult size)) (rv 0.5 0.5))
            (rest plan)
            size)))

(defn scene [plan]
  (csvg/timed
   (csvg/svg {:width width
              :height height
              :stroke "black"
              :fill-opacity "5%"
              :fill "black"
              :stroke-width 1.0}
     (shapes plan))))

(defn page []
  (let [plan (vec (repeatedly 11 gen-shape))]
    (fn []
      [:div
       [:div.canvas-frame [scene (take (:recursion-depth @ui-state) plan)]]
       [:div.contained
        [:p.center "Recursively layer regular polygons on each outward face."]
        [:p.center (view-sketch/generate :decorative-tiles)]
        (ctrl/numeric ui-state "Recursion Depth" [:recursion-depth] [1 9 1])]])))

(sketch/definition decorative-tiles
  {:created-at "2023-01-20"
   :type :svg
   :tags #{}}
  (ctrl/mount page "sketch-host"))
