(ns shimmers.sketches.zoetropic
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            quil.sketch
            [shimmers.common.framerate :as framerate]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [s 50
        buffer 36
        [w h] [(* 3 s) (* 2 s)]
        rate 24
        capture (.createCapture (quil.sketch/current-applet) "video")]
    (.size capture w h)
    (.hide capture)
    (q/frame-rate rate)
    {:width w
     :height h
     :capture capture
     :frames (vec (repeatedly buffer #(q/create-image w h)))}))

(defn update-state [{:keys [capture frames width height] :as state}]
  (update-in state
             [:frames (mod (q/frame-count) (count frames))]
             (fn [dest]
               (if capture
                 (q/copy capture dest
                         [0 0 width height]
                         [0 0 width height])
                 dest)
               dest)))

(defn draw [{:keys [frames width height]}]
  (doseq [i (range (count frames))
          :let [frame (nth frames i)]]
    (q/image frame
             (* width (mod i 6))
             (* height (int (/ i 6)))
             width height)))

(defn ^:export run-sketch []
  ;; 20210417
  (q/defsketch zoetropic
    :host "quil-host"
    :size [900 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
