(ns shimmers.sketches.spaces-divided
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.sequence :as cs]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce defo (debug/state))

(defn gen-line [bounds]
  (fn []
    (let [[a b c d] (g/edges bounds)
          [[p1 q1] [p2 q2]] (dr/rand-nth [[a c] [b d]])]
      (gl/line2 (tm/mix p1 q1 (dr/random 0.1 0.9))
                (tm/mix p2 q2 (dr/random 0.1 0.9))))))

(defn isec-point [l1 l2]
  (when-let [{:keys [type] :as hit} (g/intersect-line l1 l2)]
    (when (= type :intersect)
      {:isec (:p hit) :segments #{l1 l2}})))

(defn line-intersections [lines]
  (remove nil?
          (for [[a b] (cs/all-pairs lines)]
            (isec-point a b))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect 0.95)]
    {:bounds bounds
     :mouse (gv/vec2)
     :lines (repeatedly 6 (gen-line bounds))}))

(defn update-state [{:keys [lines bounds] :as state}]
  (let [isecs (line-intersections (into lines (map gl/line2 (g/edges bounds))))]
    (-> state
        (assoc :intersections isecs
               :edges (poly-detect/intersections->edges isecs))
        (update :mouse cq/mouse-last-position-clicked))))

;; improved but still showing backwards triangles form inset sometimes?
(defn draw-inset [shape]
  (let [inset (->> (poly-detect/inset-polygon shape 4)
                   poly-detect/split-self-intersection
                   (apply max-key g/area))]
    (when (> (g/area inset) 50)
      (cq/draw-polygon inset))))

(defn describe [{:keys [points] :as shape}]
  {:vertices points
   :self-intersecting (poly-detect/self-intersecting? shape)
   :clockwise (poly-detect/clockwise-polygon? points)
   :area (g/area shape)})

(defn calculate-polygons [edges]
  (->> edges
       poly-detect/edges->graph
       poly-detect/simple-polygons
       (mapv gp/polygon2)))

(defn inset-shapes [polygons]
  (->> (for [poly polygons
             :let [inset (poly-detect/inset-polygon poly 2.0)]]
         (cond (poly-detect/self-intersecting? inset)
               (apply max-key g/area (poly-detect/split-self-intersection inset))
               (> (g/area inset) 125)
               inset
               :else nil))
       (remove nil?)))

(defn draw [{:keys [mouse edges]}]
  (reset! defo {})
  (q/ellipse-mode :radius)
  (q/background 1.0)

  ;; either inset or polygon detection is occasionally tossing in weird outputs
  ;; sometimes inset polygons self-intersect, so need to cut that part out
  (q/stroke-weight 1.0)
  (let [shapes (-> edges
                   calculate-polygons
                   inset-shapes)]

    (swap! defo assoc :n-polygons (count shapes))

    (q/no-fill)
    (q/stroke 0.55 0.5 0.5 1.0)
    (doseq [s shapes]
      (cq/draw-polygon s))

    (q/stroke 0.0 0.5 0.0 1.0)
    (doseq [s shapes]
      (draw-inset s))

    ;; highlight points on hover + debug info
    (when-let [{:keys [points] :as shape}
               (some (fn [s] (when (g/contains-point? s mouse) s)) shapes)]
      (let [inner (poly-detect/inset-polygon shape 4)]
        (swap! defo assoc :polygon
               {:outer (describe shape)
                :inner (describe inner)})
        (when-let [isec (poly-detect/self-intersecting? inner)]
          (q/with-stroke [0.0 0.5 0.5 1.0]
            (cq/circle isec 2.0)
            (cq/draw-polygon inner))))
      (doseq [[idx p] (map-indexed vector points)]
        (q/fill (/ idx (count points)) 0.5 0.5 1.0)
        (cq/circle p 3.0)))))

(comment
  ;; example of self intersect after inset operation
  (poly-detect/self-intersecting?
   (gp/polygon2 (poly-detect/inset-polygon (mapv gv/vec2 [[383.33 202.97]
                                                          [435.44 199.85]
                                                          [404.54 355.24]
                                                          [411.73 357.02]])
                                           -10))))

(sketch/defquil spaces-divided
  :created-at "2021-12-09"
  :size [800 600]
  :on-mount #(debug/mount defo)
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
