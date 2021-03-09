(ns shimmers.sketches.superposition
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.triangle :as gt]
            [thi.ng.geom.circle :as gc]
            [thi.ng.math.core :as tm]))

(defn rel-h [p]
  (* (q/height) p))

(defn rel-w [p]
  (* (q/width) p))

(defn draw-polygon [poly]
  (cq/draw-shape (geom/vertices poly)))

(defn random-shape-at [[p1 p2] t]
  (-> (gt/triangle2 [0 0] [0 13] [17 0])
      (geom/rotate (* 2 Math/PI (rand)))
      (geom/translate (tm/mix p1 p2 t))))

(defn setup []
  (q/color-mode :hsl 360 1.0 1.0 1.0)
  (let [current (rect/rect (rel-w 0.15) (rel-h 0.15) (rel-w 0.3) (rel-h 0.3))
        target (rect/rect (rel-w 0.55) (rel-h 0.55) (rel-w 0.3) (rel-h 0.3))]
    {:current current
     :target target
     :brushes (repeatedly 64
                          (fn [] [(geom/random-point-inside current)
                                 (geom/random-point-inside target)]))
     :tween 1.0}))

(defn random-rect []
  (let [w (q/random 0.1 0.5)
        h (q/random 0.1 0.5)]
    (rect/rect (rel-w (* (- 1 w) (rand))) (rel-h (* (- 1 h) (rand)))
               (rel-w w) (rel-h h))))

(defn random-circle []
  (let [r (q/random 0.1 0.4)]
    (gc/circle (rel-w (tm/clamp (rand) r (- 1 r)))
               (rel-h (tm/clamp (rand) r (- 1 r)))
               (rel-h r))))

(defn update-state [state]
  (let [fc (q/frame-count)
        tween (mod (/ fc 500) 1.0)]
    (if (= tween 0)
      (let [target ((rand-nth [random-rect random-circle]))]
        (assoc state :current (:target state)
               :target target
               :brushes (map (fn [b] [(second b) (geom/random-point-inside target)])
                             (:brushes state))
               :tween tween))
      (assoc state :tween tween))))

(defn draw [{:keys [tween current target brushes]}]
  ;; (q/background 255)
  ;; (q/no-fill)
  ;; (q/stroke-weight 1)
  ;; (q/stroke 0 1.0 1.0 1.0)
  ;; (draw-polygon current)
  ;; (q/stroke 0 0.0 0.0 1.0)
  ;; (draw-polygon target)

  ;; (q/no-stroke)
  ;; measure/beat
  (q/stroke-weight (-> (- (q/noise (/ (q/frame-count) 600) 0.0) 0.35)
                       (tm/map-interval-clamped [0 0.65] [0 0.3])))
  (q/fill (mod (* 720 (q/noise (/ (q/frame-count) 1000) 100.0)) 360)
          0.5 0.5 0.1)
  (doseq [brush brushes]
    (draw-polygon (random-shape-at brush tween))))

(defn ^:export run-sketch []
  ;; 20210308
  (q/defsketch superposition
    :host "quil-host"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
