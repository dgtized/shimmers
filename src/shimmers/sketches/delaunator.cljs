(ns shimmers.sketches.delaunator
  (:require
   [delaunator]
   [shimmers.algorithm.delaunay :as delvor]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
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

(set! *warn-on-infer* true)

(defonce defo (debug/state))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

;; Most of the following functions are translated into ClojureScript from
;; https://mapbox.github.io/delaunator/ and then converted into thi.ng/geom type
;; records.
(defn next-half-edge [e]
  (if (= 2 (mod e 3)) (- e 2) (+ e 1)))

(defn prev-half-edge [e]
  (if (= 0 (mod e 3)) (+ e 2) (- e 1)))

(defn delaunator-from ^js/Delaunator [points]
  (js/Delaunator (clj->js points)))

(defn triangle-edges [points]
  (let [^js/Delaunator delaunay (delaunator-from points)
        triangles (.-triangles delaunay)
        half-edges (.-halfedges delaunay)]
    (for [e (range (alength triangles))
          :when (> e (aget half-edges e))
          :let [p (nth points (aget triangles e))
                q (nth points (aget triangles (next-half-edge e)))]]
      (gl/line2 p q))))

(comment
  (triangle-edges [[0 10] [0 5] [5 5] [4 2]]))

(defn delaunay-triangle [points ^js/Delaunator delaunay t]
  (let [triangles (.-triangles delaunay)
        a (aget triangles (* 3 t))
        b (aget triangles (+ (* 3 t) 1))
        c (aget triangles (+ (* 3 t) 2))]
    (gt/triangle2 (nth points a) (nth points b) (nth points c))))

(defn triangles [points]
  (let [^js/Delaunator delaunay (delaunator-from points)
        triangles (.-triangles delaunay)]
    (for [t (range (/ (alength triangles) 3))]
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
  (let [^js/Delaunator delaunay (js/Delaunator.from (clj->js points))
        half-edges (.-halfedges delaunay)]
    (for [e (range (alength (.-triangles delaunay)))
          :when (< e (aget half-edges e))]
      (gl/line2 (triangle-center points delaunay (triangle-of-edge e))
                (triangle-center points delaunay (triangle-of-edge (aget (.-halfedges delaunay) e)))))))

(defn edges-around-point [^js/Delaunator delaunay start]
  (let [half-edges (.-halfedges delaunay)]
    (loop [incoming start result []]
      (let [r (conj result incoming)
            outgoing (next-half-edge incoming)
            incoming (aget half-edges outgoing)]
        (if (and (not= incoming -1) (not= incoming start))
          (recur incoming r)
          r)))))

(defn voronoi-polygons [points]
  (let [^js/Delaunator delaunay (js/Delaunator.from (clj->js points))
        triangles (.-triangles delaunay)]
    (loop [e 0 seen #{} out []]
      (if (< e (alength triangles))
        (let [p (aget triangles (next-half-edge e))]
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

;; TODO: import and use d3-delaunay: https://github.com/d3/d3-delaunay
;; it handles clipping to bounds for voronoi and some other niceties
;; TODO: border clip voronoi polygons?
;; TODO: ensure points are stable as debug state changes, but not as n-points changes
;; TODO: add hover debug on selected polygon/point/triangle?
;; TODO: investigate why circumcenter was unhappy?
(defn delaunator-diagram [bounds state coords]
  (let [points (if (:include-bounding-corners state)
                 (concat coords (g/vertices bounds))
                 coords)
        edges (triangle-edges points)
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
    [(when (get state :show-points)
       (svg/group {:fill "black"}
                  (for [p points] (gc/circle p 1.5))))
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

(defn d3-diagram [bounds state points]
  (let [triangles (delvor/delaunay-triangles points)
        circumcenters (delvor/voronoi-circumcenters points bounds)
        polygons (delvor/voronoi-cells points bounds)]
    (reset! defo
            {:points points
             :triangles triangles
             :circumcenters circumcenters
             :voronoi polygons})
    [(when (get state :show-points)
       (svg/group {:fill "black"}
                  (for [p points] (gc/circle p 1.5))))
     (when (get state :show-triangles)
       (svg/group {:fill "none"} triangles))
     (when (get state :show-circumcenters)
       (svg/group {:fill "red"} (for [c circumcenters] (gc/circle c 1.5))))
     (when (get state :show-polygons)
       (svg/group {:stroke "blue" :fill "none"} polygons))]))

(defn scene [bounds state points]
  (let [diagram (case (:mode state)
                  :delaunator delaunator-diagram
                  :d3-delaunay d3-diagram)]
    (csvg/svg {:width width
               :height height
               :stroke "black"
               :fill "white"
               :stroke-width 0.5}
              (apply list (diagram bounds state points)))))

(defonce ui-state
  (ctrl/state {:mode :d3-delaunay
               :point-mode :random-points
               :n-points 32
               :include-bounding-corners true
               :show-points true
               :show-edges false
               :show-triangles true
               :show-circumcenters false
               :show-circumcircles false
               :show-voronoi-edges false
               :show-polygons false
               :debug false}))

(def point-modes {:random-points rp/random-points
                  :random-cells rp/random-cells
                  :random-cell-jitter rp/random-cell-jitter
                  :poisson-disc-sampling rp/poisson-disc-sampling} )

(defn generate-points [bounds ui-state]
  (let [{:keys [n-points point-mode]} @ui-state
        points (if (pos? n-points)
                 n-points
                 5)]
    ((get point-modes point-mode) bounds points)))

(defn page [bounds points]
  (let [mode (:mode @ui-state)]
    [:div
     [:div.canvas-frame [scene bounds @ui-state points]]
     [:div.flexcols
      [:div {:style {:width "20em"}}
       (view-sketch/generate :delaunator)
       [:h4 "Controls"]
       (ctrl/numeric ui-state "Generated Points" [:n-points] [2 1024 1])
       (ctrl/change-mode ui-state (keys point-modes) :point-mode)
       (ctrl/change-mode ui-state [:delaunator :d3-delaunay] :mode)
       (when (= mode :delaunator)
         (ctrl/checkbox ui-state "Include Bounding Corners" [:include-bounding-corners]))
       (ctrl/checkbox ui-state "Points" [:show-points])
       (when (= mode :delaunator)
         (ctrl/checkbox ui-state "Edges" [:show-edges]))
       (ctrl/checkbox ui-state "Triangles" [:show-triangles])
       (ctrl/checkbox ui-state "Circumcenters" [:show-circumcenters])
       (when (= mode :delaunator)
         (ctrl/checkbox ui-state "Circumcircles" [:show-circumcircles]))
       (when (= mode :delaunator)
         (ctrl/checkbox ui-state "Voronoi Edges" [:show-voronoi-edges]))
       (ctrl/checkbox ui-state "Voronoi Polygons" [:show-polygons])
       (ctrl/checkbox ui-state "Debug" [:debug])]
      (when (:debug @ui-state)
        [:div [:h4 "Debug"]
         (debug/display defo)])]]))

(sketch/definition delaunator
  {:created-at "2022-03-08"
   :type :svg
   :tags #{}}
  (let [bounds (rect/rect 0 0 width height)
        points (generate-points (g/scale-size bounds 0.99) ui-state)]
    (ctrl/mount #(page bounds points) "sketch-host")))
