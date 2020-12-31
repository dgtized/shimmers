(ns shimmers.sketches.waves
  "Based on https://thecodingtrain.com/CodingChallenges/102-2d-water-ripple.html"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.framerate :as framerate]
            [shimmers.macros.loop :as loop :include-macros true]
            [thi.ng.ndarray.core :as nd]))

(defn make-buffer [width height]
  (let [zeros (repeatedly (* width height) (fn [] (float 0)))]
    (nd/ndarray :float32 zeros [width height])))

(defn setup []
  (let [width (/ (q/width) 3)
        height (/ (q/height) 3)]
    {:width width
     :height height
     :buffer (make-buffer width height)
     :previous (make-buffer width height)}))

(defn update-state [{:keys [width height buffer previous] :as state}]
  (when (= 0 (mod (q/frame-count) 30))
    (let [i (int (q/random width))
          j (int (q/random height))]
      (println (str "droplet at " i " " j))
      (nd/set-at buffer i j 1)))
  (loop/c-for [j 1 (< j (- height 1)) (inc j)
               i 1 (< i (- width 1)) (inc i)]
    (nd/set-at buffer i j
               (* 0.9
                  (- (/ (+ (nd/get-at previous (inc i) j)
                           (nd/get-at previous (dec i) j)
                           (nd/get-at previous i (inc j))
                           (nd/get-at previous i (dec j)))
                        2)
                     (nd/get-at buffer i j)))))
  (assoc state
         :buffer previous
         :previous buffer))

(defn draw [{:keys [width height buffer]}]
  (q/background 255 32)
  (q/no-stroke)
  (loop/c-for [j 0 (< j height) (inc j)
               i 0 (< i width) (inc i)]
    (let [amount (nd/get-at buffer i j)]
      (when (> amount 0)
        (q/fill 128 128 (+ (* amount 64) 128) 16)
        (q/rect (* 3 i) (* 3 j) 3 3)))))

(defn ^:export run-sketch []
  (q/defsketch waves
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))


