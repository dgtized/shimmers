(ns shimmers.sketches.typography
  (:require [clojure.string :as str]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]))

(defn setup []
  (q/frame-rate 5)
  {:text ["This is how the world ends, not with a bang, but with a whimper."
          "This is often how the world ends, not with a bang, but with a whimper."
          "This is slowly how the world ends, not with a bang, but with a whimper."
          "This is how the world ends, this is how the world ends, not with a bang, but with a whimper."
          "With a whimper."
          "With a bang."
          "and so it goes"
          "This is only a test of the emergency broadcast system."]})

(defn update-state [state]
  state)

;; TODO; typewriter effect with delays over multiple draw cycles
(defn draw [{:keys [text]}]
  (q/background 255 (rand-nth [16 64 128 255 200]))
  (q/fill 0 255)
  ;; (q/text-font (rand-nth fonts))
  (let [startx 20
        starty (+ 50 (* 50 (rand-nth [0 0 0 0 0 1 2])))
        x (atom startx)
        y (atom starty)]
    (doseq [word (str/split (rand-nth text) #"\s+")]
      (let [style (rand-nth [:normal :italic :bold :bolditalic])
            size (rand-nth [10 15 20 25 30])
            text-width (q/text-width (str word " "))]
        (q/rotate (rand-nth [0 0 0 0 0 0 0 0 0.02 -0.02 -0.05]))
        (q/text-style style)
        (q/text-size size)
        (q/text word @x @y)
        (let [yset (rand-nth [0 0 0 0 0 20 40])]
          (cond
            (> (+ @x text-width) (+ 20 (q/width)))
            (do (swap! y + 20) (reset! x starty))
            (and (> yset 0) (>= @x starty) (>= @x 150))
            (swap! x - (rand-nth [0 30 50 50]))
            :else
            (swap! x + (rand-nth [-10 50 text-width text-width 100])))
          (swap! y + yset))))))

(sketch/defquil typography
  :created-at "2021-01-11"
  :size [600 400]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
