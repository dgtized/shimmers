(ns shimmers.sketches.spaces-divided
  (:require
   [clojure.set :as set]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.polygon-detection :as poly-detect]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.sequence :as cs]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.dstruct.core :as d]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.utils.intersect :as isec]
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
      [(:p hit) #{l1 l2}])))

(defn line-intersections [lines]
  (remove nil?
          (for [[a b] (cs/all-pairs lines)]
            (isec-point a b))))

(defn vertices-per-isec
  "Calculate the set of vertices for each line from the intersections."
  [intersections]
  (->> (for [[p lines] intersections
             line lines]
         {line #{p}})
       (apply (partial merge-with set/union))))

(defn intersections->edges [isecs]
  (apply set/union
         (for [[{[a b] :points} vertices] (vertices-per-isec isecs)]
           (let [ordered (sort-by (fn [p] (g/dist a p)) (conj vertices a b))]
             (->> ordered
                  dedupe ;; sometimes a or b is already in points
                  (partition 2 1)
                  ;; ensure edges are always low pt -> high pt
                  (map (fn [v] (sort v)))
                  set)))))

;; extracted from thi.ng.geom.polygon to address bugs
;; http://alienryderflex.com/polygon_inset/
(defn- inset-corner
  [prev curr next d]
  (let [[dx1 dy1 :as d1] (tm/- curr prev)
        [dx2 dy2 :as d2] (tm/- next curr)
        d1 (tm/mag d1) d2 (tm/mag d2)]
    (if-not (or (tm/delta= 0.0 d1) (tm/delta= 0.0 d2))
      (let [i1 (tm/* (tm/* (gv/vec2 dy1 (- dx1)) (/ d1)) d) ;; TODO avoid double multiply => (/ d d1)
            i2 (tm/* (tm/* (gv/vec2 dy2 (- dx2)) (/ d2)) d) ;; TODO ditto => (/ d d2)
            c1 (tm/+ curr i1)
            c2 (tm/+ curr i2)
            prev (tm/+ prev i1)
            next (tm/+ next i2)]
        (swap! defo assoc-in [:inset [prev curr next]]
               (isec/intersect-line2-line2? prev c1 c2 next))
        (if (tm/delta= c1 c2)
          c1 (get (isec/intersect-line2-line2? prev c1 c2 next) :p)))
      curr)))

;; references:
;; https://stackoverflow.com/questions/1109536/an-algorithm-for-inflating-deflating-offsetting-buffering-polygons

(defn inset-polygon
  "For CW polygons, use positive distance to inset or negative to outset.
  For CCW polygons, use opposite."
  [points d]
  (mapv
   (fn [[p c n]] (inset-corner p c n d))
   (d/successive-nth 3 (d/wrap-seq points [(last points)] [(first points)]))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect 0.9)]
    {:bounds bounds
     :mouse (gv/vec2)
     :lines (repeatedly 6 (gen-line bounds))}))

(defn update-state [{:keys [lines bounds] :as state}]
  (let [isecs (line-intersections (into lines (map gl/line2 (g/edges bounds))))]
    (assoc state
           :mouse (cq/mouse-position)
           :intersections isecs
           :edges (intersections->edges isecs))))

(defn draw-inset [shape]
  (let [inset (gp/inset-polygon (:points shape) -4)]
    (when (not (poly-detect/self-intersecting? (gp/polygon2 inset)))
      (cq/draw-shape inset))))

(defn draw [{:keys [mouse edges]}]
  (reset! defo {})
  (q/ellipse-mode :radius)
  (q/background 1.0)

  ;; either inset or polygon detection is occasionally tossing in weird outputs
  ;; sometimes inset polygons self-intersect, so need to cut that part out
  (q/stroke-weight 1.0)
  (let [polygons (->> edges
                      poly-detect/edges->graph
                      poly-detect/simple-polygons)
        shapes (->> (for [poly (map gp/polygon2 polygons)
                          :let [inset (gp/polygon2 (gp/inset-polygon (:points poly) -6.0))]]
                      (cond (and (> (g/area inset) 1000)
                                 (not (poly-detect/self-intersecting? inset)))
                            inset
                            (> (g/area poly) 50)
                            poly
                            :else nil))
                    (remove nil?))]

    (swap! defo assoc :n-polygons (count shapes))
    (when-let [{:keys [points] :as shape}
               (some (fn [s] (when (g/contains-point? s mouse) s)) shapes)]
      (swap! defo assoc :polygon
             {:p points
              :self-intersecting (poly-detect/self-intersecting? shape)
              :clockwise (poly-detect/clockwise-polygon? points)
              :area (g/area shape)}))

    (q/stroke 0.55 0.5 0.5 1.0)
    (doseq [s shapes]
      (cq/draw-polygon s))

    (q/stroke 0.0 0.5 0.0 1.0)
    (doseq [s shapes]
      (draw-inset s))))

(comment
  ;; example of self intersect after inset operation
  (poly-detect/self-intersecting?
   (gp/polygon2 (inset-polygon (mapv gv/vec2 [[383.33 202.97]
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
