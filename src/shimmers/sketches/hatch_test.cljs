(ns shimmers.sketches.hatch-test
  (:require
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.equations :as eq]))

(def width 800)
(def height 800)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn examples []
  [(fn [{[w _] :size :as cell}]
     (let [rect (-> (rect/rect (/ w 2))
                    (g/center (g/centroid cell)))]
       (into [rect]
             (clip/hatch-rectangle rect (dr/random 4 12) (dr/random-tau)
                                   [(dr/random) (dr/random)]))))
   (fn [{[w _] :size :as cell}]
     (let [rect (-> (rect/rect (/ w 2))
                    (g/center (g/centroid cell)))
           theta (dr/random-tau)]
       (into [(geometry/rotate-around-centroid rect theta)]
             (mapv (fn [line] (geometry/rotate-around line (g/centroid rect) theta))
                   (clip/hatch-rectangle rect (dr/random 4 12) (dr/random-tau)
                                         [(dr/random) (dr/random)])))))
   (fn [{[w _] :size :as cell}]
     (let [rect (-> (rect/rect (/ w 2))
                    (g/center (g/centroid cell)))
           spacing (dr/random 4 16)
           theta (dr/random-tau)
           theta0 (dr/random-tau)
           theta1 (+ theta0 (dr/gaussian (* eq/TAU 0.25) 0.1))]
       (concat [(geometry/rotate-around-centroid rect theta)]
               (mapv (fn [line] (geometry/rotate-around line (g/centroid rect) theta))
                     (clip/hatch-rectangle rect spacing theta0
                                           [(dr/random) (dr/random)]))
               (mapv (fn [line] (geometry/rotate-around line (g/centroid rect) theta))
                     (clip/hatch-rectangle rect spacing theta1
                                           [(dr/random) (dr/random)])))))

   (fn [{[w _] :size :as cell}]
     (let [rect (-> (rect/rect (/ w 2))
                    (g/center (g/centroid cell)))
           theta (dr/random-tau)
           theta0 (dr/random-tau)]
       (concat [(geometry/rotate-around-centroid rect theta)]
               (mapv (fn [line] (geometry/rotate-around line (g/centroid rect) theta))
                     (clip/hatch-rectangle rect (dr/random 4 12) theta0
                                           [(dr/random) (dr/random)]))
               (mapv (fn [line] (geometry/rotate-around line (g/centroid rect) theta))
                     (clip/hatch-rectangle rect (dr/random 4 12) theta0
                                           [(dr/random) (dr/random)])))))])

(defn shapes [bounds examples]
  (for [[cell example] (map vector (g/subdivide bounds {:num 2})
                            examples)]
    (csvg/group {}
      (example cell))))

(defn scene [{:keys [scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.5}
    (shapes (csvg/screen width height)
            (examples))))

(sketch/definition hatch-test
  {:created-at "2025-01-22"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
