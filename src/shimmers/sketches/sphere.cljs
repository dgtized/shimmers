(ns shimmers.sketches.sphere
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.math.core :as tm]))

(defn sphere-points [vertice-count]
  (for [j (range vertice-count)
        i (range vertice-count)
        :let [longitude (tm/map-interval i 0 vertice-count (- Math/PI) Math/PI)
              latitude (tm/map-interval j 0 vertice-count (- (/ Math/PI 2)) (/ Math/PI 2))]]
    [(* (Math/sin longitude) (Math/cos latitude))
     (* (Math/sin longitude) (Math/sin latitude))
     (Math/cos longitude)]))

(def ordering-fns
  [shuffle
   (partial sort-by first)
   (partial sort-by second)
   (partial sort-by last)
   (partial sort-by (juxt first second))
   (partial sort-by (juxt first last))
   (partial sort-by (juxt second first))
   (partial sort-by (juxt second last))
   (partial sort-by (juxt last first))
   (partial sort-by (juxt last second))
   (partial sort-by (fn [p] (* (first p) (last p))))
   ;; FIXME: this sort is unstable, and dependent on previous ordering
   (partial sort (fn [a b]
                   (let [crossA (- (* (first a) (second b)) (* (first b) (second a)))
                         crossB (- (* (second a) (last b)) (* (second b) (last a)))]
                     (> crossA crossB))))])

(defn vertice-update [{:keys [active-count vertices] :as state}]
  (if (and (<= active-count 1) (> (q/millis) 2000))
    (assoc state :vertices ((rand-nth ordering-fns) vertices))
    state))

(defn setup []
  {:active-count 10
   :vertices (sphere-points 32)
   :radius 150})

(defn update-state [{:keys [vertices] :as state}]
  (-> state
      (assoc :active-count (int (tm/mix-cosine 0 (count vertices) (/ (q/millis) 8000))))
      vertice-update))

(defn draw [{:keys [radius active-count vertices]}]
  (q/orbit-control)
  (q/background 255)
  (q/stroke 0)
  (q/stroke-weight 1.25)
  (q/rotate-x 0.6)
  (q/rotate-y -0.2)
  (q/rotate-z (/ (q/frame-count) 1000))
  (doseq [position (take active-count vertices)]
    (apply q/point (map (partial * radius) position))))

(sketch/defquil sphere
  :created-at "2021-01-30"
  :size [600 400]
  :renderer :p3d
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
