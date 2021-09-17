(ns shimmers.sketches.reaction-diffusion
  "From https://www.karlsims.com/rd.html and https://ciphrd.com/2019/08/24/reaction-diffusion-on-shader/."
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.shader :as shader]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.probability :as p]
            [shimmers.sketch :as sketch :include-macros true]))

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
  (let [scale 1.0
        [width height] [(* scale (q/width)) (* scale (q/height))]]
    {:image-size [width height]
     :in-buffer (starting-conditions (q/create-graphics width height :p2d)
                                     width height)
     :out-buffer (q/create-graphics width height :p3d)
     :shader (q/load-shader "shaders/reaction-diffusion.main.frag.c"
                            "shaders/reaction-diffusion.vert.c")
     :display-shader (q/load-shader "shaders/reaction-diffusion.display.frag.c"
                                    "shaders/reaction-diffusion.vert.c")}))

(def modes {:abs-difference 0
            :concentration-a 1
            :concentration-b 2})

(defonce ui-state
  (ctrl/state {:droplets false
               :diffusion-a 1.0
               :diffusion-b 0.1
               :feed 0.065
               :kill 0.062
               :delta-t 1.0
               :mode :abs-difference
               :invert false}))

(defn controls []
  [:div
   [:div (ctrl/checkbox ui-state "Add Droplets Randomly" [:droplets])]
   [:div [:h3 "Parameters"]
    (ctrl/numeric ui-state "Diffusion A" [:diffusion-a] [0.0 1.0 0.001])
    (ctrl/numeric ui-state "Diffusion B" [:diffusion-b] [0.0 1.0 0.001])
    (ctrl/numeric ui-state "Feed Rate" [:feed] [0.0 1.0 0.001])
    (ctrl/numeric ui-state "Kill Rate" [:kill] [0.0 1.0 0.001])
    (ctrl/numeric ui-state "𝚫t" [:delta-t] [0.0 2.0 0.001])]
   [:div [:h3 "Display Mode"]
    (ctrl/change-mode ui-state (keys modes) :mode)
    (ctrl/checkbox ui-state "Invert" [:invert])]])

;; Cribbed some of the feedback loop from https://medium.com/@edoueda/integrating-p5-js-and-webgl-with-react-js-96c848a63170
(defn update-state [{:keys [image-size shader in-buffer out-buffer] :as state}]
  (let [[w h] image-size
        {:keys [droplets diffusion-a diffusion-b feed kill delta-t]} @ui-state]

    (when (and droplets (p/chance 0.01))
      (let [size (+ 4 (* 38 (rand)))]
        (q/with-graphics in-buffer
          (if (< (rand) 0.5)
            (do
              (q/no-fill)
              (q/stroke 0.0 1.0 0.0 1.0))
            (do
              (q/no-stroke)
              (q/fill 1.0 0.0 0.0 1.0)))
          (cq/circle (* w (rand)) (* h (rand)) size))))

    (shader/transform shader out-buffer in-buffer [w h]
                      {"resolution" (array w h)
                       "concentrations" in-buffer
                       "diffusionA" diffusion-a
                       "diffusionB" diffusion-b
                       "feed" feed
                       "kill" kill
                       "deltaT" delta-t})
    state))

(defn draw [{:keys [in-buffer display-shader]}]
  (let [{:keys [mode invert]} @ui-state]
    (when (q/loaded? display-shader)
      (shader/pass display-shader [(q/width) (q/height)]
                   {"image" in-buffer
                    "mode" (get modes mode)
                    "invert" invert}))))

(sketch/defquil reaction-diffusion
  :created-at "2021-09-15"
  :tags #{:shader}
  :on-mount (fn [] (ctrl/mount controls))
  :size [800 600]
  :renderer :p3d
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
