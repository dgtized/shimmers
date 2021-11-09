(ns shimmers.sketches.plaid-arrangements
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.algorithm.square-packing :as square]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.sequence :as cs]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.color :as color]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(defn pack-grid [region steps]
  (let [pack
        (fn pack [rects]
          (let [divide (dr/weighted-by g/area rects)
                split (dr/weighted {(square/row-major divide) 4
                                    :clockwise 2
                                    :all 1})
                pos [(dr/random 0.3 0.7)
                     (dr/random 0.3 0.7)]
                ratio (/ 1 tm/PHI)
                panes (square/proportional-split divide ratio pos split)]
            (into (remove #{divide} rects) panes)))]
    (cs/iterate-cycles steps pack [region])))

(def palettes
  (->> ["https://artsexperiments.withgoogle.com/artpalette/colors/c64408-399768-d2ad31"
        "https://artsexperiments.withgoogle.com/artpalette/colors/421f13-bd7c47-7e3118"
        "https://artsexperiments.withgoogle.com/artpalette/colors/926548-364b44-667565"
        "https://artsexperiments.withgoogle.com/artpalette/colors/cbe4c5-3b7c4a-c1776c"
        "https://artsexperiments.withgoogle.com/artpalette/colors/f3d69b-573c28-c59d60"
        "https://artsexperiments.withgoogle.com/artpalette/colors/2a5e5e-2c2a39-86725e"]
       (map color/url->palette)
       (into [[[0.99 0.5 0.5 0.6]
               [0.6 0.8 0.5 0.9]
               [0.0 0.8 0.25 0.3]]])))

;; TODO: extend grid generation to include grids from square-pack, ie not even subdivisions
(defn setup []
  (q/noise-seed (dr/random-int 100000))
  (q/color-mode :hsl 1.0)
  (let [region (cq/screen-rect 0.95)
        theta (dr/random 0.6 1.2)
        [color-a color-b color-c]
        (mapv (fn [c alpha] (assoc c 3 alpha))
              (dr/shuffle (map #(into [] %) (dr/rand-nth palettes)))
              (vec (dr/shuffle [0.7 0.9 0.5])))]
    {:grids [{:grid (dr/random-sample 0.85 (g/subdivide region {:rows 17 :cols 19}))
              :stroke-weight 0.6
              :cell-color color-a
              :noise-threshold 0.55
              :noise-scale 0.02
              :theta theta
              :spacing 8}
             {:grid (dr/random-sample 0.90 (g/subdivide region {:rows 11 :cols 13}))
              :stroke-weight 0.9
              :cell-color color-b
              :noise-threshold 0.65
              :noise-scale 0.005
              :theta (+ theta (dr/random 0.5 1.0))
              :spacing 12}
             {:grid (dr/random-sample 0.8 (pack-grid region 5))
              :stroke-weight 3.0
              :cell-color color-c
              :noise-threshold 0.7
              :noise-scale 0.002
              :theta (+ theta (dr/random 1.0 2.0))
              :spacing 12}]}))

(defn update-state [state]
  state)

(defn scaled-noise [pos scale]
  (let [[x y] (tm/* pos scale)]
    (q/noise x y)))

(defn draw [{:keys [grids]}]
  (q/background 1.0)
  (q/no-fill)
  (doseq [{:keys [grid noise-threshold noise-scale
                  stroke-weight cell-color
                  theta spacing]} grids]
    (doseq [r grid
            :let [center-r (g/centroid r)]]
      (apply q/stroke cell-color)
      (q/stroke-weight stroke-weight)
      (cq/draw-polygon r)
      (q/stroke-weight (* 0.5 stroke-weight))
      (when (> (scaled-noise center-r noise-scale) noise-threshold)
        (doseq [{[p q] :points} (clip/hatch-rectangle r spacing theta [0.5 0.5])]
          (q/line p q))))))

(sketch/defquil plaid-arrangements
  :created-at "2021-11-07"
  :tags #{:deterministic :static}
  :on-mount #(ctrl/mount (fn [] [:p.center (view-sketch/generate :plaid-arrangements)]))
  :size [1000 1000]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
