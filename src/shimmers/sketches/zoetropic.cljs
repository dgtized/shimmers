(ns shimmers.sketches.zoetropic
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            quil.sketch
            [reagent.core :as r]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.sequence :as cs]
            [shimmers.common.ui.controls :as ctrl]))

(def modes [:modular :delayed :random])
(defonce ui-state (r/atom {:mode :modular}))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [s 50
        buffer 36
        [w h] [(* 3 s) (* 2 s)]
        rate 12
        capture (.createCapture (quil.sketch/current-applet) "video")]
    (.size capture w h)
    (.hide capture)
    (q/frame-rate rate)
    {:width w
     :height h
     :capture capture
     :frames (vec (repeatedly buffer #(q/create-image w h)))}))

(defn active-mode [{:keys [frames]}]
  (case (:mode @ui-state)
    :modular [0 (mod (q/frame-count) (count frames))]
    :delayed [-1 0]
    :random [0 (rand-int (count frames))]))

(defn copy-frame [capture width height dest]
  (if capture
    (do (q/copy capture dest
                [0 0 width height]
                [0 0 width height])
        dest)
    dest))

(defn update-state [{:keys [capture width height] :as state}]
  (let [[r offset] (active-mode state)]
    (-> state
        (update :frames (comp vec (partial cs/rotate r)))
        (update-in [:frames offset]
                   (partial copy-frame capture width height)))))

(defn draw [{:keys [frames width height]}]
  (doseq [i (range (count frames))
          :let [frame (nth frames i)]]
    (q/image frame
             (* width (mod i 6))
             (* height (int (/ i 6)))
             width height)))

(defn ^:export run-sketch []
  ;; 20210417
  (ctrl/mount (partial ctrl/change-mode ui-state modes))
  (q/defsketch zoetropic
    :host "quil-host"
    :size [900 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
