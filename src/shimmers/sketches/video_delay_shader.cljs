(ns shimmers.sketches.video-delay-shader
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.shader :as shader]
   [shimmers.common.video :as video]
   [shimmers.sketch :as sketch :include-macros true]))

(def history 26)

(defn setup []
  (let [[w h] [640 480]]
    {:dims [w h]
     :camera (video/capture w h)
     :shader (q/load-shader "shaders/video-delay-shader.frag.c"
                            "shaders/video-shader.vert.c")
     :frames (vec (repeatedly history #(q/create-image w h)))}))

(defn update-state [{:keys [dims camera] :as state}]
  (let [fc (q/frame-count)
        [w h] dims]
    (update-in state [:frames (mod fc history)]
               (partial video/copy-frame camera w h))))

(defn draw [{:keys [dims shader frames]}]
  (let [[w h] dims
        fc (q/frame-count)
        frame (q/create-image w h)]
    (when (and (q/loaded? shader) (> fc 1))
      (q/copy (nth frames (mod fc history)) frame [0 0 w h] [0 0 w h])
      (shader/pass shader [w h]
                   {"u_resolution" (array w h)
                    "u_time" (/ (q/millis) 1000.0)
                    "frame" frame
                    "frame10" (if (> fc 10) (nth frames (mod (- fc 10) history)) frame)
                    "frame25" (if (> fc 10) (nth frames (mod (- fc 25) history)) frame)
                    }))))

;; out of memories after N seconds sometimes?
(sketch/defquil video-delay-shader
  :created-at "2023-01-03"
  :tags #{:camera :shader :genuary2023}
  :size [640 480]
  :renderer :p3d
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
