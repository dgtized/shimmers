(ns shimmers.sketches.radar
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defrecord Particle [position velocity])

(defn step [{:keys [position velocity] :as particle}]
  (-> particle
      (assoc :position (v/add position velocity))
      (update :position (fn [pos] (v/wrap2d pos (q/width) (q/height))))))

(defn green [x] [0 230 0 x])

(defn setup []
  (let [hw (/ (q/width) 2)
        hh (/ (q/height) 2)
        radius (* 0.45 (q/height))]
    (q/background 0)
    {:theta 0.0
     :center (gv/vec2 hw hh)
     :radius radius
     :particles [(->Particle (gv/vec2 0 60) (gv/vec2 0.15 0.02))
                 (->Particle (gv/vec2 (* 2 hw) 60) (gv/vec2 -0.2 0.02))
                 (->Particle (gv/vec2 hw 0) (gv/vec2 -0.02 0.1))
                 (->Particle (gv/vec2 120 0) (gv/vec2 -0.05 0.15))]
     :contacts []}))

(defn contact-hit [{:keys [theta radius center]} point]
  (let [translated-point (tm/- point center)
        heading (g/heading translated-point)
        mtheta (mod theta eq/TAU)
        delta (- heading mtheta)
        tolerance 0.01]
    (when (and (< (g/dist (gv/vec2 0 0) translated-point) radius)
               (< (- tolerance) delta tolerance))
      ;; (println [heading mtheta delta translated-point])
      {:position translated-point :lifespan 200})))

(defn update-state [state]
  (let [new-contacts
        (keep (fn [{:keys [position]}]
                (contact-hit state position))
              (:particles state))]
    (-> state
        (update :contacts
                (fn [contacts]
                  (->> (into contacts new-contacts)
                       (map #(update % :lifespan - 1))
                       (filter #(> (:lifespan %) 0)))))
        (update :theta + (/ eq/TAU (* 60 15)))
        (update :particles (partial map step)))))

(defn draw
  [{:keys [contacts theta center radius]}]
  (apply q/translate center)

  (when (= 0 (mod (q/frame-count) 8))
    (q/background 0 6))
  (apply q/stroke (green 255))
  (q/stroke-weight 2)
  (q/line 0 0 (* radius (q/cos theta)) (* radius (q/sin theta)))

  (doseq [{:keys [position lifespan]} contacts]
    (apply q/stroke (green lifespan))
    (apply q/point position)))

(defn page []
  (sketch/component
   :size [600 400]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition radar
  {:created-at "2021-01-16"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
