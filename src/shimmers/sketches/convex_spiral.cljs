(ns shimmers.sketches.convex-spiral
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.geometry :as geometry]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.polygon :as gp]))

(defn explanation []
  [:div
   [:h4 "Concept"]
   [:p
    "Draw a clockwise spiral through a set of random points in a plane."
    [:ol
     [:li "Generate a set of random points in a plane, and an empty path."]
     [:li "Draw a "
      [:a {:href "https://en.wikipedia.org/wiki/Convex_hull"} "convex hull"]
      " around those points."]
     [:li "Remove the points on the hull from the set in the plane and add them to the path in clockwise order."]
     [:li "Repeat from step 2 on the remaining points until only 2 points remain."]
     [:li "Display the path through each of the convex hulls connected end to end."]]]])

;; FIXME: sometimes there is overlap of an outer path and an inner?
;; Might just be at transition from outer to inner hull?
;; also is there a way to handle the last 3 to continue spiral?
(defn convex-spiral [points]
  (loop [path [] points points]
    (if (< (count points) 3)
      (into path points)
      (let [hull (gp/convex-hull* points)]
        (recur (into path hull)
               (remove (set hull) points))))))

(defn setup []
  (q/no-loop)
  {:points (geometry/generate-points 64 #(q/random 0.15 0.85))})

(defn draw [{:keys [points]}]
  (q/background 255)
  (q/stroke-weight 0.5)
  (q/ellipse-mode :radius)
  (q/fill 0 0 0)
  (doseq [p (map cq/rel-pos points)]
    (cq/circle p 1))

  (doseq [[p q] (partition 2 1 (convex-spiral (map cq/rel-pos points)))]
    (q/line p q)))

(sketch/defquil convex-spiral-sketch
  :created-at "2021-03-22"
  :tags #{:static}
  :on-mount #(ctrl/mount explanation)
  :size [800 600]
  :setup setup
  :draw draw
  :middleware [m/fun-mode framerate/mode])
