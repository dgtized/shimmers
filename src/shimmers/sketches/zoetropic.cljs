(ns shimmers.sketches.zoetropic
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.sequence :as cs]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.video :as video]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]))

(def modes [:modular :delayed :rewind :chance-rewind :random])
(defonce ui-state (ctrl/state {:mode :modular}))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [s 50
        buffer 36
        [w h] [(* 3 s) (* 2 s)]
        rate 12]
    (q/frame-rate rate)
    {:capture (video/capture w h)
     :width w
     :height h
     :frames (vec (repeatedly buffer #(q/create-image w h)))}))

(defn active-mode [{:keys [frames]}]
  (case (:mode @ui-state)
    :modular [0 (mod (q/frame-count) (count frames))]
    :delayed [-1 0]
    :rewind [1 (dec (count frames))]
    :chance-rewind
    (dr/weighted {[-1 0] 5
                  [(dr/random-int 8) (+ 6 (dr/random-int 12))] 1})
    :random [0 (dr/random-int (count frames))]))

(defn update-state [{:keys [capture width height] :as state}]
  (let [[r offset] (active-mode state)]
    (-> state
        (update :frames (comp vec (partial cs/rotate r)))
        (update-in [:frames offset]
                   video/copy-frame capture width height))))

(defn draw [{:keys [frames width height]}]
  (doseq [i (range (count frames))
          :let [frame (nth frames i)]]
    (q/image frame
             (* width (mod i 6))
             (* height (int (/ i 6)))
             width height)))

(defn page []
  [sketch/with-explanation
   (sketch/component
    :size [900 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [ctrl/change-mode ui-state modes]])

(sketch/definition zoetropic
  {:created-at "2021-04-17"
   :tags #{:camera}
   :type :quil}
  (ctrl/mount page))
