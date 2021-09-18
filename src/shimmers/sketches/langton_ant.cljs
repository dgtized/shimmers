(ns shimmers.sketches.langton-ant
  "References:
   * https://en.wikipedia.org/wiki/Langton%27s_ant
   * https://thecodingtrain.com/CodingChallenges/089-langtonsant.html"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.math.vector :as v]
            [shimmers.sketch :as sketch :include-macros true]))

(comment
  ;; Would like to use non-floating point coords but it appears to break
  ;; the "unzipping" event, not sure how to find that otherwise.
  (def north (v/vec2 0 1))
  (def south (v/vec2 0 -1))
  (def east (v/vec2 1 0))
  (def west (v/vec2 -1 0))

  (defn turn-right [[x y]] (v/vec2 y (- x)))
  (defn turn-left [[x y]] (v/vec2 (- y) x)))

(defn turn-right [dir] (+ dir (/ Math/PI 2)))
(defn turn-left [dir] (- dir (/ Math/PI 2)))

(def default-grid-cell false)

(defn create-ant [position direction]
  {:position position :direction direction})

(defn setup []
  {:grid {}
   :ants [(create-ant (v/vec2 0 0) (/ Math/PI 2))
          (create-ant (v/vec2 2 0) 0)]})

(defn move-ant [grid {:keys [position direction] :as ant}]
  (let [pixel (get grid (:position ant) default-grid-cell)
        new-dir ((if pixel turn-right turn-left) direction)
        ;; FIXME: possible propagating floating point error?
        new-pos (v/add position (v/vec2 (Math/cos new-dir)
                                        (Math/sin new-dir)))]
    [(assoc grid position (not pixel))
     (assoc ant :position new-pos :direction new-dir)]))

(defn update-state [{:keys [grid ants]}]
  (loop [grid grid ants ants processed []]
    (if (seq ants)
      (let [[new-grid new-ant] (move-ant grid (first ants))]
        (recur new-grid (rest ants) (conj processed new-ant)))
      {:grid grid :ants processed})))

(defn grid-range [grid]
  (let [active-cells (keep (fn [[pos v]] (when v pos)) grid)
        xs (map first active-cells)
        ys (map second active-cells)]
    {:x [(reduce min xs) (reduce max xs)]
     :y [(reduce min ys) (reduce max ys)]}))

(defn draw [{:keys [grid]}]
  (q/background 255)
  (q/fill 0)
  (q/rect-mode :center)
  (let [{[x0 x1] :x [y0 y1] :y} (grid-range grid)
        center (v/vec2 (/ (q/width) 2) (/ (q/height) 2))
        r (/ (+ (q/width) (q/height))
             (+ 20 (* 2 (+ (- x1 x0) (- y1 y0)))))]
    (doseq [[position value] grid]
      (when value
        (let [[x y] (v/add center (v/scale position r))]
          (q/rect x y r r))))))

(sketch/defquil langton-ant
  :created-at "2020-12-23"
  :size [600 400]
  :setup setup
  :update update-state
  :draw draw
  :middleware [framerate/mode m/fun-mode])
