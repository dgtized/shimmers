(ns shimmers.sketches.boolean-grid
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.quadtree :as saq]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn tree-annotate [{:keys [circles] :as state}]
  (let [tree (saq/add-to-circletree (saq/circletree (cq/screen-rect)) circles)]
    (assoc state
           :tree tree
           :traversal (saq/simple-traversal-tree tree))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [circles (map-indexed (fn [i p]
                               (let [r (/ (tm/clamp (dr/gaussian 1.0 0.4) 0.1 2.0) 16.0)
                                     on (dr/chance 0.3)]
                                 (vary-meta (gc/circle p (* 0.5 r (q/height)))
                                            assoc
                                            :id i
                                            :state on
                                            :energy (if on 1.0 0.0))))
                             (rp/poisson-disc-sampling (cq/screen-rect 0.875) 150))]
    (tree-annotate {:circles circles})))

(defn xor [a b]
  (cond (and a b) false
        a true
        b true
        :else false))

(defn force-push
  [{:keys [p r] :as circle} bounds neighborhood v]
  (let [close (g/closest-point bounds p)
        pressure (tm/normalize (tm/- p close) (/ (* 1.01 r) (g/dist p close)))
        forces (reduce (fn [f n]
                         (let [c (g/get-point-data n)]
                           (tm/+ f (tm/normalize (tm/- p (:p c))
                                                 (/ (+ r (:r c)) (g/dist p (:p c)))))))
                       (gv/vec2) neighborhood)]
    (g/translate circle (tm/* (tm/+ (tm/* forces (/ 1.0 (count neighborhood))) pressure) v))))

(defn update-circle-state [circles tree]
  (for [c circles]
    (let [neighborhood (rest (saq/k-nearest-neighbors tree 5 (:p c)))
          mc (meta c)
          on (:state mc)
          energy (:energy mc)
          neighbor-states
          (for [neighbor neighborhood]
            (:state (meta (g/get-point-data neighbor))))
          next-on (if (< 0.02 energy 0.98)
                    on
                    (xor on (reduce xor neighbor-states)))]
      (vary-meta (force-push c (g/bounds tree) neighborhood (if on (dr/gaussian 1.5 0.5) 0.5))
                 assoc
                 :state next-on
                 :energy (tm/mix* energy (if next-on 1.0 0.0)
                                  (dr/random 0.025 0.1))))))

(defn update-neighborhood [{:keys [tree] :as state}]
  (update state :circles update-circle-state tree))

(defn update-state [state]
  (tree-annotate (update-neighborhood state)))

(defn draw [{:keys [circles tree traversal]}]
  (q/ellipse-mode :radius)
  (q/background 1.0)
  (q/stroke 0.0 1.0)
  (doseq [circle circles]
    (qdg/draw circle))

  (q/stroke 0.0 0.2)
  (doseq [{:keys [r] :as node} traversal
          :let [circle (:d node)]]
    (q/no-fill)
    (let [neighbors (rest (saq/k-nearest-neighbors tree 5 (:p circle)))]
      (comment (qdg/draw (apply rect/rect r)))
      (doseq [overlap (keep (fn [neighbor]
                              (when (collide/overlaps? circle
                                                       (g/get-point-data neighbor))
                                (g/get-point-data neighbor)))
                            neighbors)]
        (qdg/draw (gl/line2 (:p circle) (:p overlap))))
      (q/fill 0.0 (* (:energy (meta circle)) 0.25))
      (qdg/draw (g/scale-size circle 0.9)))))

(defn page []
  [:div
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   [:div.centered.readable-width
    [:p "Genuary 2026 - Day 7 - Boolean Logic"]
    [:p "The circles are all force pushing eachother around, but only if they are 'on'. They flip their energy from on to off based on an " [:em "xor"] " operation with their closest 3 neighbors."]]])

(sketch/definition boolean-grid
  {:created-at "2026-01-07"
   :tags #{:genuary2026}
   :type :quil}
  (ctrl/mount page))
