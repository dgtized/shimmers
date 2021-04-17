(ns shimmers.sketches.zoetropic
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            quil.sketch
            [shimmers.common.framerate :as framerate]))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [[w h] [100 66]
        rate 12
        capture (.createCapture (quil.sketch/current-applet) "video")]
    (.size capture w h)
    (.hide capture)
    (q/frame-rate rate)
    {:width w
     :height h
     :capture capture
     :frames (vec (repeatedly (* rate 3) #(q/create-image w h)))}))

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
  (let [[w h] [width height]]
    (doseq [i (range (count frames))
            :let [frame (nth frames i)]]
      (when frame
        (q/image frame (* w (mod i 6)) (* h (int (/ i 6)))
                 w h)))))

(defn ^:export run-sketch []
  ;; 20210417
  (q/defsketch zoetropic
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
