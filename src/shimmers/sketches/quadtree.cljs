(ns shimmers.sketches.quadtree
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.geometry :as geometry]
   [shimmers.algorithm.quadtree :as saq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.spatialtree :as spatialtree]
   [thi.ng.geom.vector :as gv]))

(defonce defo (debug/state))

(defn build-tree [{:keys [points] :as state}]
  (assoc state :tree
         (reduce (fn [t {:keys [p] :as c}] (g/add-point t p c))
                 (spatialtree/quadtree 0 0 (q/width) (q/height))
                 points)))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect 0.99)]
    (build-tree {:bounds bounds
                 :points (repeatedly 256 #(gc/circle (g/random-point-inside bounds)
                                                     (dr/random-int 2 12)))
                 :tree (spatialtree/quadtree bounds)
                 :mouse (gv/vec2)})))

(defn update-state [state]
  (assoc state :mouse (cq/mouse-position)))

(defn draw-complete-tree [{:keys [tree]}]
  (let [traversal (tree-seq (fn [t] (not-empty (spatialtree/get-children t)))
                            (fn [t] (remove nil? (spatialtree/get-children t)))
                            tree)]
    (doseq [n traversal]
      (q/stroke 0.5)
      (cq/rectangle (g/bounds n))
      (when-let [circle (g/get-point-data n)]
        (q/stroke 0.0)
        (cq/rectangle (g/bounds n))
        (q/stroke 0.0 0.5 0.5)
        (cq/circle circle)))))

(defonce ui-state (ctrl/state {:isec-mode :circles-overlap}))

(def intersect-modes {:intersect-shape g/intersect-shape
                      :circles-overlap geometry/circles-overlap?})

(defn draw-path-to-selection [{:keys [points tree mouse]}]
  (let [overlap-isec (get intersect-modes (get @ui-state :isec-mode))
        cursor (gc/circle mouse 5)]
    (q/stroke 0.0 0.5 0.5)
    (doseq [circle points]
      (cq/circle circle))
    (q/stroke 0.6 0.5 0.5)
    (cq/circle cursor)
    (let [matches (->> [tree]
                       (saq/lazy-select-quad
                        #(g/intersect-shape cursor %)
                        #(overlap-isec cursor (g/get-point-data %))))]
      (if (seq matches)
        (doseq [{:keys [p] :as selected} matches]
          (q/stroke 0.5 0.8 0.5)
          (cq/circle selected)
          (let [path-bounds (map g/bounds (spatialtree/path-for-point tree p))]
            (swap! defo update :matches conj
                   {:selected selected
                    :path path-bounds
                    ;; FIXME: this is not quite right because it's only selected if cursor contains the point
                    :intersection (g/intersect-shape cursor selected)})
            (q/stroke 0 0 0)
            (doseq [r path-bounds]
              (cq/rectangle r))))
        (let [neighbor (saq/nearest-neighbor tree mouse)]
          (q/stroke 0.75 0.8 0.5)
          (q/line mouse neighbor)
          (swap! defo assoc :matches
                 {:neighbor neighbor}))))))

(defn draw [{:keys [bounds mouse] :as state}]
  (reset! defo {:matches []})
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/stroke-weight 0.66)
  (q/no-fill)
  (swap! defo assoc :mouse mouse)
  (if (g/contains-point? bounds mouse)
    (draw-path-to-selection state)
    (draw-complete-tree state)))

(defn ui-controls []
  [:div.flexcols
   [:div
    (ctrl/change-mode ui-state (keys intersect-modes)
                      {:mode-key :isec-mode})]
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
