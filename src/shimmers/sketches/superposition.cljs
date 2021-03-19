(ns shimmers.sketches.superposition
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq :refer [rel-h rel-w]]
            [shimmers.math.geometry :as geometry]
            [shimmers.math.probability :as p]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.math.core :as tm]))

(defn draw-polygon [poly]
  (cq/draw-shape (geom/vertices poly)))

(defn random-shape-at [[p1 p2] t spin]
  (-> (gt/triangle2 [0 0] [0 13] [17 0])
      (geom/rotate (* 2 Math/PI (if spin
                            (mod (* t 200) 3)
                            (rand))))
      (geom/translate (tm/mix p1 p2 t))))

(defn random-triangle []
  (let [s (q/random 0.1 0.2)]
    (-> (gt/triangle2 [0 0] [0 (rel-h s)] [(rel-w s) 0])
        (geometry/rotate-around-centroid (* 2 Math/PI (rand)))
        (geom/translate (cq/rel-pos (q/random 0.2 0.8)
                                    (q/random 0.2 0.8))))))

(defn random-rect []
  (let [w (q/random 0.1 0.3)
        h (q/random 0.1 0.3)]
    (-> (rect/rect (rel-w (* (- 1 w) (rand))) (rel-h (* (- 1 h) (rand)))
                   (rel-w w) (rel-h h))
        (geometry/rotate-around-centroid (* 2 Math/PI (rand))))))

(defn random-circle []
  (let [r (q/random 0.1 0.3)]
    (gc/circle (cq/rel-pos (tm/clamp (rand) r (- 1 r))
                           (tm/clamp (rand) r (- 1 r)))
               (rel-h r))))

(defn random-target []
  ((rand-nth [random-rect random-circle random-triangle])))

(defn var-rate [n]
  (Math/sin (* (/ Math/PI 2) n)))

(defn setup []
  (q/color-mode :hsl 360 1.0 1.0 1.0)
  (let [current (random-target)
        target (random-target)]
    {:current current
     :target target
     :brushes (repeatedly 64
                          (fn [] [(geom/random-point-inside current)
                                 (geom/random-point-inside target)]))
     :base 0
     :spin (p/chance 0.5)
     :interval 500}))

(defn update-state [{:keys [base interval] :as state}]
  (let [fc (q/frame-count)]
    (if (= (- fc base) interval)
      (let [target (random-target)]
        (assoc state :current (:target state)
               :target target
               :brushes (map (fn [b] [(second b) (geom/random-point-inside target)])
                             (:brushes state))
               :base fc
               :interval (q/floor (q/random 200 600))
               :spin (p/chance 0.5)
               :tween 0.0))
      (assoc state :tween (var-rate (/ (- fc base) interval))))))

(defn draw [{:keys [tween current target brushes spin]}]
  ;; (q/background 255)
  ;; (q/no-fill)
  ;; (q/stroke-weight 1)
  ;; (q/stroke 0 1.0 1.0 1.0)
  ;; (draw-polygon current)
  ;; (q/stroke 0 0.0 0.0 1.0)
  ;; (draw-polygon target)

  ;; (q/no-stroke)
  ;; measure/beat
  (let [fc (q/frame-count)]
    (q/stroke 0 0 0 0.35)
    (q/stroke-weight (-> (- (q/noise (/ fc 600) 0.0) 0.35)
                         (tm/map-interval-clamped [0 0.65] [0 0.6])))
    (q/fill (mod (* 1080 (q/noise (/ fc 3000) 100.0)) 360)
            (tm/map-interval (q/noise (/ fc 800) 200.0) [0 1] [0.4 1.0])
            (tm/map-interval (q/noise (/ fc 800) 400.0) [0 1] [0.45 1.0])
            0.035))
  (doseq [brush brushes]
    (draw-polygon (random-shape-at brush tween spin))))

(defn ^:export run-sketch []
  ;; 20210308
  (q/defsketch superposition
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
