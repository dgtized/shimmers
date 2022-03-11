(ns shimmers.sketches.delaunator
  (:require
   [delaunator]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.triangle :as gt]
   [thi.ng.geom.vector :as gv]))

(defonce defo (debug/state))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn gen-point []
  (rv (dr/random 0.05 0.95) (dr/random 0.05 0.95)))

;; Most of the following functions are translated into ClojureScript from
;; https://mapbox.github.io/delaunator/ and then converted into thi.ng/geom type
;; records.
(defn next-half-edge [e]
  (if (= 2 (mod e 3)) (- e 2) (+ e 1)))

(defn prev-half-edge [e]
  (if (= 0 (mod e 3)) (+ e 2) (- e 1)))

(defn triangle-edges [points]
  (let [delaunay (js/Delaunator.from (clj->js points))]
    (for [e (range (alength (.-triangles delaunay)))
          :when (> e (aget (.-halfedges delaunay) e))
          :let [p (nth points (aget (.-triangles delaunay) e))
                q (nth points (aget (.-triangles delaunay) (next-half-edge e)))]]
      (gl/line2 p q))))

(comment
  (triangle-edges [[0 10] [0 5] [5 5] [4 2]]))

(defn delaunay-triangle [points delaunay t]
  (let [a (aget (.-triangles delaunay) (* 3 t))
        b (aget (.-triangles delaunay) (+ (* 3 t) 1))
        c (aget (.-triangles delaunay) (+ (* 3 t) 2))]
    (gt/triangle2 (nth points a) (nth points b) (nth points c))))

(defn triangles [points]
  (let [delaunay (js/Delaunator.from (clj->js points))]
    (for [t (range (/ (alength (.-triangles delaunay)) 3))]
      (delaunay-triangle points delaunay t))))

(comment (triangles [[0 10] [0 5] [5 5] [4 2]]))

;; Something about this is wrong, but not clear what, so using gt/circumcircle
(defn circumcenter [[ax ay] [bx by] [cx cy]]
  (let [ad (+ (* ax ax) (* ay ay))
        bd (+ (* bx bx) (* by by))
        cd (+ (* cx cx) (* cy cy))
        D (* 2 (+ (* ax (- by cy))
                  (* bx (- cy ay))
                  (* cx (- ay by))))]
    (gv/vec2 (* (/ 1 D)
                (+ (* ad (- by cy))
                   (* bd (- cy ay))
                   (* cd (- ay cy))))
             (* (/ 1 D)
                (+ (* ad (- cx bx))
                   (* bd (- ax cx))
                   (* cd (- bx ax)))))))

(defn triangle-of-edge [e]
  (Math/floor (/ e 3)))

(defn triangle-center [points delaunay t]
  (let [{[a b c] :points} (delaunay-triangle points delaunay t)]
    (first (gt/circumcircle-raw a b c))))

(defn voronoi-edges [points]
  (let [delaunay (js/Delaunator.from (clj->js points))]
    (for [e (range (alength (.-triangles delaunay)))
          :when (< e (aget (.-halfedges delaunay) e))]
      (gl/line2 (triangle-center points delaunay (triangle-of-edge e))
                (triangle-center points delaunay (triangle-of-edge (aget (.-halfedges delaunay) e)))))))

(defn edges-around-point [delaunay start]
  (loop [incoming start result []]
    (let [r (conj result incoming)
          outgoing (next-half-edge incoming)
          incoming (aget (.-halfedges delaunay) outgoing)]
      (if (and (not= incoming -1) (not= incoming start))
        (recur incoming r)
        r))))

(defn voronoi-polygons [points]
  (let [delaunay (js/Delaunator.from (clj->js points))]
    (loop [e 0 seen #{} out []]
      (if (< e (alength (.-triangles delaunay)))
        (let [p (aget (.-triangles delaunay) (next-half-edge e))]
          (if (contains? seen p)
            (recur (inc e) seen out)
            (let [edges (edges-around-point delaunay e)
                  triangles (map triangle-of-edge edges)
                  vertices (for [t triangles]
                             (triangle-center points delaunay t))]
              (recur (inc e) (conj seen p)
                     (conj out (gp/polygon2 vertices))))))
        out))))

(comment (voronoi-polygons [[0 0] [10 0] [10 10] [0 10] [5 5]]))

;; TODO: border clip voronoi polygons?
;; TODO: ensure points are stable as debug state changes, but not as n-points changes
;; TODO: add hover debug on selected polygon/point/triangle?
;; TODO: investigate why circumcenter was unhappy?
(defn diagram [state points]
  (let [edges (triangle-edges points)
        triangles (triangles points)
        circumcircles (for [{[a b c] :points} triangles]
                        (gt/circumcircle a b c))
        voronoi-edges (voronoi-edges points)
        polygons (voronoi-polygons points)]
    (reset! defo
            {:points points
             :edges edges
             :circumcircles circumcircles
             :voronoi-edges voronoi-edges
             :polygons polygons
             :bad-polygons (filter (fn [{:keys [points]}] (<= (count points) 2)) polygons)})
    [(svg/group {:fill "black"}
                (for [p points] (gc/circle p 1.5)))
     (when (get state :show-edges)
       (svg/group {} edges))
     (when (get state :show-triangles)
       (svg/group {:fill "none"} triangles))
     (when (get state :show-circumcenters)
       (svg/group {:fill "red"}
                  (for [{:keys [p]} circumcircles] (gc/circle p 1.5))))
     (when (get state :show-circumcircles)
       (svg/group {:fill "none" :stroke "red" :stroke-width 0.2} circumcircles))
     (when (get state :show-voronoi-edges)
       (svg/group {:stroke "blue"} voronoi-edges))
     (when (get state :show-polygons)
       (svg/group {}
                  (svg/group {:stroke "blue" :fill "none"}
                             (filter (fn [{points :points}] (> (count points) 2))
                                     polygons))
                  (svg/group {:fill "green" :stroke "green"}
                             (for [{:keys [points]} (filter (fn [{points :points}] (<= (count points) 2))
                                                            polygons)]
                               (let [[p q] points]
                                 (svg/group {}
                                            (gc/circle p 3.0)
                                            (when q (gc/circle q 3.0))
                                            (when (= 2 (count points))
                                              (gl/line2 p q))))))))]))

(defn scene [state points]
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.5}
            (apply list (diagram state points))))

(defonce ui-state
  (ctrl/state {:n-points 12
               :show-edges false
               :show-triangles true
               :show-circumcenters false
               :show-circumcircles false
               :show-voronoi-edges false
               :show-polygons false
               :debug false}))

(defn generate-points [ui-state]
  (let [{:keys [n-points]} @ui-state
        points (if (pos? n-points)
                 n-points
                 5)]
    (concat (repeatedly points gen-point)
            (g/vertices (rect/rect 0 0 width height)))))

(defn page [points]
  [:div
   [:div.canvas-frame [scene @ui-state points]]
   [:div.flexcols
    [:div {:style {:width "18em"}}
     (view-sketch/generate :delaunator)
     [:h4 "Controls"]
     (ctrl/numeric ui-state "Generated Points" [:n-points] [2 256 1])
     (ctrl/checkbox ui-state "Edges" [:show-edges])
     (ctrl/checkbox ui-state "Triangles" [:show-triangles])
     (ctrl/checkbox ui-state "Circumcenters" [:show-circumcenters])
     (ctrl/checkbox ui-state "Circumcircles" [:show-circumcircles])
     (ctrl/checkbox ui-state "Voronoi Edges" [:show-voronoi-edges])
     (ctrl/checkbox ui-state "Voronoi Polygons" [:show-polygons])
     (ctrl/checkbox ui-state "Debug" [:debug])]
    (when (:debug @ui-state)
      [:div [:h4 "Debug"]
       (debug/display defo)])]])

(sketch/definition delaunator
  {:created-at "2022-03-08"
   :type :svg
   :tags #{}}
  (let [points (generate-points ui-state)]
    (ctrl/mount #(page points) "sketch-host")))
