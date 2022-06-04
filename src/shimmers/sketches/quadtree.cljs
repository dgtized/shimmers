(ns shimmers.sketches.quadtree
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.quadtree :as saq]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.spatialtree :as spatialtree]
   [thi.ng.geom.vector :as gv]))

(defonce defo (debug/state))
(defonce ui-state (ctrl/state {:rebuild false
                               :isec-mode :circles-overlap
                               :tree-mode :circletree
                               :mouse-hover true
                               :knearest 1}))

(def intersect-modes {:intersect-shape g/intersect-shape
                      :circles-overlap geometry/circles-overlap?})

(def tree-modes {:circletree saq/circletree
                 :quadtree spatialtree/quadtree})

(defn build-tree [bounds points]
  (let [tree (get tree-modes (:tree-mode @ui-state))]
    (reduce (fn [t {:keys [p] :as c}] (g/add-point t p c))
            (tree bounds) points)))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect 0.99)
        points (repeatedly 256 #(gc/circle (g/random-point-inside bounds)
                                           (dr/random-int 8 32)))]
    {:bounds bounds
     :points points
     :tree (build-tree bounds points)
     :mouse (gv/vec2)}))

(defn update-state [state]
  (if (:rebuild @ui-state)
    (do
      (swap! ui-state update :rebuild not)
      (let [{:keys [bounds points]} state]
        (assoc state :tree (build-tree bounds points))))
    (update state :mouse cq/mouse-last-position-clicked (:mouse-hover @ui-state))))

(defn draw-complete-tree [{:keys [tree]}]
  (let [depth (satisfies? IMeta tree)
        traversal
        (if depth
          (tree-seq (fn [t] (not-empty (spatialtree/get-children t)))
                    (fn [t] (map #(vary-meta % assoc :depth (inc (:depth (meta t))))
                                (remove nil? (spatialtree/get-children t))))
                    (vary-meta tree assoc :depth 0))
          (tree-seq (fn [t] (not-empty (spatialtree/get-children t)))
                    (fn [t] (remove nil? (spatialtree/get-children t)))
                    tree))]
    (doseq [n traversal]
      (q/stroke 0.5)
      (cq/rectangle (g/bounds n))
      (when-let [circle (g/get-point-data n)]
        (q/stroke 0.0)
        (when depth
          (q/fill (/ 16.0 (Math/pow 2 (:depth (meta n)))) 0.1))
        (cq/rectangle (g/bounds n))
        (q/no-fill)
        (q/stroke 0.0 0.5 0.5)
        (cq/circle circle)))))

(defn list-nearest-neighbors [tree k mouse]
  (let [neighbors (if (> k 1)
                    (saq/k-nearest-neighbors tree k mouse)
                    [(saq/nearest-neighbor-node tree mouse)])]
    (doseq [neighbor neighbors
            :let [p (g/get-point neighbor)]]
      (q/stroke 0.75 0.8 0.5)
      (q/line mouse p))

    (swap! defo assoc :nearest-neighbor
           (mapv (fn [n]
                   (let [p (g/get-point n)]
                     {:p p
                      :d (g/dist mouse p)
                      :data (g/get-point-data n)}))
                 neighbors))))

(defn show-closest-circle [tree cursor]
  (when-let [closest (saq/closest-circle tree cursor)]
    (q/stroke 0.45 0.5 0.5)
    (cq/rectangle (g/bounds closest))

    (swap! defo assoc :closest
           {:circle closest
            :center-dist (g/dist (:p cursor) (:p closest))
            :overlap-dist (saq/circle-overlap closest cursor)})))

(defn draw-path-to-selection [{:keys [points tree mouse]}]
  (let [overlap-isec (get intersect-modes (get @ui-state :isec-mode))
        cursor (gc/circle mouse 5)
        k (get @ui-state :knearest)]
    (q/stroke 0.0 0.5 0.5)
    (doseq [circle points]
      (cq/circle circle))
    (q/stroke 0.6 0.5 0.5)
    (cq/circle cursor)

    (list-nearest-neighbors tree k mouse)
    (show-closest-circle tree cursor)

    (let [matches (->> [tree]
                       (saq/lazy-select-quad
                        #(g/intersect-shape cursor %)
                        #(overlap-isec cursor (g/get-point-data %))))]
      (when (seq matches)
        (doseq [{:keys [p] :as selected} matches]
          (q/stroke 0.5 0.8 0.5)
          (cq/circle selected)
          (let [point-path (spatialtree/path-for-point tree p)
                path-bounds (map g/bounds point-path)]
            (swap! defo update :matches conj
                   {:selected selected
                    :path (mapv (fn [t] (let [b (g/bounds t)]
                                         [b (g/get-point-data t)]))
                                point-path)
                    ;; FIXME: this is not quite right because it's only selected if cursor contains the point
                    :intersection (g/intersect-shape cursor selected)})
            (q/stroke 0 0 0)
            (doseq [r path-bounds]
              (cq/rectangle r))))))))

(defn draw [{:keys [bounds mouse] :as state}]
  (reset! defo {:mouse mouse :nearest-neighbor {} :matches []})
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/stroke-weight 0.66)
  (q/no-fill)
  (if (g/contains-point? bounds mouse)
    (draw-path-to-selection state)
    (draw-complete-tree state)))

(defn ui-controls []
  [:div.flexcols
   [:div
    (ctrl/checkbox-after ui-state "Mouse Hover" [:mouse-hover])
    (ctrl/change-mode ui-state (keys intersect-modes)
                      {:mode-key :isec-mode})
    (ctrl/change-mode ui-state (keys tree-modes)
                      {:mode-key :tree-mode
                       :on-change
                       (fn [] (swap! ui-state assoc :rebuild true))})
    (ctrl/numeric ui-state "K-nearest" [:knearest] [1 16 1])]
   [:div (debug/display defo)]])

(sketch/defquil quadtree
  :created-at "2021-10-10"
  :tags #{:datastructures}
  :size [800 600]
  :on-mount #(ctrl/mount ui-controls)
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
