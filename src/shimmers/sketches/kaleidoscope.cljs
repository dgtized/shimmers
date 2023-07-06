(ns shimmers.sketches.kaleidoscope
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.shader :as shader]
   [shimmers.common.video :as video]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.sequence :as cs]))

(def history 26)

(def modes
  [:basic,:rgb-delay])

(defonce ui-state (ctrl/state {:mode :basic :blades 5.0}))

(defn setup []
  (let [[w h] [320 240]]
    {:mode :color-delay
     :dims [w h]
     :camera (video/capture w h)
     :shader (q/load-shader "shaders/kaleidoscope.frag.c"
                            "shaders/video-shader.vert.c")
     :buffer (q/create-image w h)
     :frames (vec (repeatedly history #(q/create-image w h)))}))

(defn update-state [{:keys [dims camera] :as state}]
  (let [fc (q/frame-count)
        [w h] dims]
    (update-in state [:frames (mod fc history)]
               video/copy-frame camera w h)))

(defn draw [{:keys [dims shader buffer frames]}]
  (let [[w h] dims
        fc (q/frame-count)]
    (when (and (q/loaded? shader) (> fc 4))
      ;; not clear why copy is required but it is -- otherwise every frame is black
      (q/copy (nth frames (mod fc history)) buffer [0 0 w h] [0 0 w h])
      (shader/pass shader [w h]
                   {"u_resolution" (array w h)
                    "u_time" (/ (q/millis) 1000.0)
                    "u_mode" (cs/index-of modes (:mode @ui-state))
                    "blades" (float (:blades @ui-state))
                    "frame" (nth frames (mod (- fc 1) history))
                    "frame10" (if (> fc 10) (nth frames (mod (- fc 10) history)) buffer)
                    "frame25" (if (> fc 25) (nth frames (mod (- fc 25) history)) buffer)
                    }))))

(defn page []
  [sketch/with-explanation
   (sketch/component
    :size [640 480]
    :renderer :p3d
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:div.ui-controls
    [ctrl/change-mode ui-state modes]
    [ctrl/numeric ui-state "Blades" [:blades] [1.0 13.0 1.0]]]])

;; out of memories after N seconds sometimes?
(sketch/definition kaleidoscope
  {:created-at "2023-07-02"
   :tags #{:camera :shader}
   :type :quil}
  (ctrl/mount page))
