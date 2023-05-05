(ns shimmers.sketches.square-packing
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.square-packing :as square]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.sequence :as cs]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(def modes [:row-major :row :column :clockwise :counter-clockwise :all])

(defonce ui-state
  (ctrl/state {:max-iterations 256
               :algorithm "row-major"
               :show-squares true
               :square-padding true
               :show-remaining true}))

;; Further Experiments: pack resulting squares with patterns of their own?
;; Colors and shapes, even tilted or "hand drawn" squares?

(defn middle-out
  "Alternate distribution for px,py"
  []
  (dr/weighted {0.0 1.0
                0.5 1.0
                1.0 1.0}))

(defn random-ratio []
  (dr/weighted {(/ 1 tm/PHI) 4
                0.5 2
                (/ 1 3) 2}))

(defn pack-step
  [{:keys [remaining squares square-limit pick-rectangle ratio position algorithm] :as state}]
  (if (and (not-empty remaining) (< (count squares) square-limit))
    (let [rect (pick-rectangle remaining)
          [square & panes]
          (square/proportional-split rect (ratio rect) (position rect) (algorithm rect))]
      (-> state
          (assoc :remaining (into (remove #{rect} remaining) panes))
          (update :squares conj square)))
    state))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [{:keys [max-iterations algorithm]} @ui-state]
    {:square-limit max-iterations
     :pick-rectangle (partial dr/weighted-by g/area)
     :position #(repeatedly 2 (fn [] (mod (* tm/PHI (dr/random)) 1.0)))
     :ratio (constantly (/ 1 tm/PHI))
     :algorithm (let [mode (keyword algorithm)]
                  (if (= mode :row-major)
                    square/row-major
                    (constantly mode)))
     :squares []
     :remaining [(cq/screen-rect 0.98)]}))

(defn update-state [state]
  (cs/iterate-cycles 12 pack-step state))

(defn draw [{:keys [squares remaining]}]
  (let [{:keys [show-remaining show-squares square-padding]} @ui-state
        scale (if square-padding (/ 1 tm/PHI) 1.0)]
    (q/background 1.0)
    (q/stroke-weight 0.66)
    (when show-remaining
      (q/stroke 0.35 0.5 0.5 0.5)
      (q/fill 1.0 1.0)
      (doseq [rects remaining]
        (cq/draw-polygon rects)))
    (when show-squares
      (q/stroke 0.0 0.0 0.0 1.0)
      (q/fill 1.0 0.1)
      (doseq [square squares]
        (-> square
            (g/scale-size scale)
            cq/draw-polygon)))))

(defn ui-controls []
  [:div.flexcols
   (ctrl/container
    (ctrl/slider ui-state (fn [v] (str "Max Iterations " v)) [:max-iterations] [1 1280 1.0])
    (ctrl/dropdown ui-state "Split Approach" [:algorithm]
                   (zipmap (map name modes) modes))
    (ctrl/checkbox ui-state "Show Squares" [:show-squares])
    (when (:show-squares @ui-state)
      (ctrl/checkbox ui-state "Padding on Square" [:square-padding]))
    (ctrl/checkbox ui-state "Show Remaining Rectangle" [:show-remaining]))
   [:div (view-sketch/generate :square-packing)]])

(defn page []
  [sketch/with-explanation
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [ui-controls]])

(sketch/definition square-packing
  {:created-at "2021-10-17"
   :tags #{:deterministic}
   :type :quil}
  (ctrl/mount page))
