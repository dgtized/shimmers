(ns shimmers.sketches.reaction-diffusion
  "From https://www.karlsims.com/rd.html and https://ciphrd.com/2019/08/24/reaction-diffusion-on-shader/. Also some tricks from http://colordodge.com/ReactionDiffusion/."
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.shader :as shader]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]))

(def modes {:abs-difference 0
            :concentration-a 1
            :concentration-b 2
            :binary 3
            :color-angle 4})

(defonce ui-state
  (ctrl/state {:texture-scale 1.0
               :droplets false
               :diffusion-a 1.0
               :diffusion-b 0.35
               :feed 0.044
               :kill 0.066
               :delta-t 1.0
               :iterations 16
               :mode :abs-difference
               :invert false}))

(defn starting-conditions [image width height]
  (q/with-graphics image
    (q/color-mode :rgb 1.0)
    (q/background 1.0 0.0 0.0 1.0)
    (q/no-fill)
    (q/stroke 0.0 1.0 0.0 1.0)
    (q/rect (* 0.3 width) (* 0.4 height) (* 0.4 width) (* 0.2 height))
    (q/ellipse (/ width 2) (/ height 2) 16 16))
  image)

(defn setup []
  (q/color-mode :rgb 1.0)
  (let [scale (:texture-scale @ui-state)
        [width height] [(* scale (q/width)) (* scale (q/height))]]
    {:image-size [width height]
     :in-buffer (starting-conditions (q/create-graphics width height :p2d)
                                     width height)
     :out-buffer (q/create-graphics width height :p3d)
     :shader (q/load-shader "shaders/reaction-diffusion.main.frag.c"
                            "shaders/reaction-diffusion.vert.c")
     :display-shader (q/load-shader "shaders/reaction-diffusion.display.frag.c"
                                    "shaders/reaction-diffusion.vert.c")}))

(defn ui-controls []
  (ctrl/container
   [:div
    (ctrl/numeric ui-state "Texture Scale (requires restart)" [:texture-scale] [0.1 5.0 0.1])
    (ctrl/checkbox ui-state "Add Droplets Randomly" [:droplets])]
   [:div [:h3 "Parameters"]
    (ctrl/numeric ui-state "Diffusion A" [:diffusion-a] [0.0 1.0 0.001])
    (ctrl/numeric ui-state "Diffusion B" [:diffusion-b] [0.0 1.0 0.001])
    (ctrl/numeric ui-state "Feed Rate" [:feed] [0.0 1.0 0.001])
    (ctrl/numeric ui-state "Kill Rate" [:kill] [0.0 1.0 0.001])
    (ctrl/numeric ui-state "𝚫t" [:delta-t] [0.0 2.0 0.001])
    (ctrl/numeric ui-state "Iterations per frame" [:iterations] [1.0 64.0 1.0])]
   [:div [:h3 "Display Mode"]
    (ctrl/change-mode ui-state (keys modes))
    (ctrl/checkbox ui-state "Invert" [:invert])]))

;; Cribbed some of the feedback loop from https://medium.com/@edoueda/integrating-p5-js-and-webgl-with-react-js-96c848a63170
(defn update-state [{:keys [image-size shader in-buffer out-buffer] :as state}]
  (let [[w h] image-size
        {:keys [droplets diffusion-a diffusion-b feed kill delta-t iterations]} @ui-state]

    (when (and droplets (dr/chance 0.01))
      (let [size (+ 4 (* 38 (dr/random)))]
        (q/with-graphics in-buffer
          (if (dr/chance 0.5)
            (do
              (q/no-fill)
              (q/stroke 0.0 1.0 0.0 1.0))
            (do
              (q/no-stroke)
              (q/fill 1.0 0.0 0.0 1.0)))
          (cq/circle (dr/random w) (dr/random h) size))))

    (dotimes [_ iterations]
      (shader/transform shader out-buffer in-buffer [w h]
                        {"resolution" (array w h)
                         "concentrations" in-buffer
                         "diffusionA" diffusion-a
                         "diffusionB" diffusion-b
                         "feed" feed
                         "kill" kill
                         "deltaT" delta-t}))
    state))

(defn draw [{:keys [in-buffer display-shader]}]
  (let [{:keys [mode invert]} @ui-state]
    (when (q/loaded? display-shader)
      (shader/pass display-shader [(q/width) (q/height)]
                   {"image" in-buffer
                    "mode" (get modes mode)
                    "invert" invert}))))

(defn page []
  [:div
   (sketch/component
    :size [900 600]
    :renderer :p3d
    :settings #(q/pixel-density 1)
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:div.contained
    [ui-controls]]])

(sketch/definition reaction-diffusion
  {:created-at "2021-09-15"
   :tags #{:shader}
   :type :quil}
  (ctrl/mount page))
