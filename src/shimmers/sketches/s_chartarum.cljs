(ns shimmers.sketches.s-charatarum
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.math.core :as tm]))

(defn make-spot [pos max-radius growth]
  {:pos pos :radius 0.01 :max-radius max-radius :growth growth})

(defn setup []
  (q/color-mode :hsl 1.0)
  {:spots []
   :t 0})

(defn update-spots [dt spots]
  (map (fn [{:keys [max-radius growth] :as spot}]
         (update spot :radius
                 (fn [radius]
                   (if (< radius max-radius)
                     (+ radius (* dt growth))
                     radius))))
       spots))

(defn remove-dead [spots]
  (remove (fn [{:keys [radius max-radius]}]
            (> radius max-radius))
          spots))

(defn add-spots [spots]
  (if (and (< (count spots) 32) (dr/chance 0.05))
    (conj spots
          (make-spot (cq/rel-vec (dr/random) (dr/random))
                     (cq/rel-h (dr/random 0.025 0.2))
                     (dr/random 2.0 4.0)))
    spots))

(defn update-state [{:keys [t] :as state}]
  (let [dt (dr/random 0.03 0.15)]
    (if (< t 100)
      (-> state
          (update :t + dt)
          (update :spots (comp
                          remove-dead
                          add-spots
                          (partial update-spots dt))))
      state)))

(defn draw [{:keys [spots]}]
  (q/ellipse-mode :radius)
  (q/no-fill)
  (q/stroke-weight 0.5)
  (doseq [{:keys [pos radius max-radius]} spots]
    (q/stroke 0.0 (+ 0.1 (* 0.3 (tm/smoothstep* 0.2 0.95 (/ radius max-radius)))))
    (cq/circle pos radius)))

(sketch/defquil s-charatarum
  :created-at "2023-01-30"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
