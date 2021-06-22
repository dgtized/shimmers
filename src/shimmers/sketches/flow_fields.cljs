(ns shimmers.sketches.flow-fields
  "https://tylerxhobbs.com/essays/2020/flow-fields"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.math.deterministic-random :as dr]
            [shimmers.math.vector :as v]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(def flows-per-iter 100)
(def settings
  (ctrl/state {:calc-points "flow-points"
               :draw "curves"
               :snap-resolution 0
               :iterations 90
               :step-size 4
               :stroke-weight 8
               :length 32
               :noise-div 6
               :jitter 0}))

(defn dir-at
  [[x y] noise-div]
  (* tm/TWO_PI (q/noise (/ x noise-div) (/ y noise-div))))

(defn draw-grid [size noise-div]
  (let [w (/ (q/width) size)
        h (/ (q/height) size)]
    (doseq [[p dir]
            (for [x (range (* -2 size) (* (+ 3 w) size) size)
                  y (range (* -2 size) (* (+ 3 h) size) size)]
              [(gv/vec2 x y) (dir-at [x y] noise-div)])]
      (q/line p (v/add p (v/polar (* 0.5 size) dir))))))

(defn snap-to [theta resolution]
  (if (> resolution 0)
    (* (Math/round (/ theta resolution)) resolution)
    theta))

(defn flow-points
  [p {:keys [step-size length noise-div snap-resolution jitter]}]
  (reductions
   (fn [p]
     (let [dir (-> (dir-at p noise-div)
                   (snap-to snap-resolution))]
       (tm/+ (tm/+ p (v/polar step-size dir))
             (v/jitter (tm/random jitter)))))
   p (range length)))

(defn angles [r resolution]
  (map (fn [theta] (v/polar r theta))
       (range 0 tm/TWO_PI resolution)))

(comment (angles 1 (/ Math/PI 6)))

(defn downhill [[x y] r noise-div snap-resolution]
  (let [surroundings
        (for [[dx dy] (angles r (if (> snap-resolution 0)
                                  (/ snap-resolution 2)
                                  (/ tm/TWO_PI 60)))]
          [[dx dy]
           (q/noise (/ (+ x dx) noise-div)
                    (/ (+ y dy) noise-div))])
        [[px py] minimum] (apply min-key second surroundings)]
    (when (> (q/noise (/ x noise-div) (/ y noise-div)) minimum)
      (gv/vec2 px py))))

(defn downhill-points
  [p {:keys [step-size length noise-div snap-resolution jitter]}]
  (reductions
   (fn [p]
     (if-let [next-point (downhill p step-size noise-div snap-resolution)]
       (tm/+ (tm/+ (tm/+ p next-point)) (v/jitter (tm/random jitter)))
       (reduced p)))
   p (range length)))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/background 1.0)
  (q/noise-seed (dr/random 1000000))
  (let [{:keys [iterations draw calc-points snap-resolution stroke-weight
                length step-size noise-div jitter]}
        @settings]
    {:iter 0
     :iterations iterations
     :calc-points (get {"flow-points" flow-points
                        "downhill-points" downhill-points}
                       calc-points)
     :snap-resolution snap-resolution
     :step-size step-size
     :stroke-weight (/ 1 stroke-weight)
     :noise-div (Math/pow 2 noise-div)
     :draw draw
     :length length
     :jitter (* step-size (if (> jitter 0) (/ 1 jitter) 0))}))

(defn update-state [state]
  (update state :iter inc))

(defn points
  [{:keys [calc-points] :as settings}]
  (calc-points (gv/vec2 (cq/rel-pos (dr/random) (dr/random)))
               settings))

(defn draw
  [{:keys [stroke-weight step-size iter iterations draw] :as settings}]
  ;; (q/stroke-weight 0.1)
  ;; (q/stroke 0.0 0.0 0.0 1.0)
  ;; (draw-grid 10 noise-div)
  (q/stroke-weight stroke-weight)
  (q/no-fill)
  (q/stroke 0.0 0.0 0.0 1.0)
  (q/ellipse-mode :radius)
  (when (< iter iterations)
    (let [hstep (* step-size 0.5)]
      (case draw
        "segments"
        (dotimes [_ flows-per-iter]
          (q/begin-shape)
          (doseq [[x y] (points settings)]
            (q/vertex x y))
          (q/end-shape))
        "curves"
        (dotimes [_ flows-per-iter]
          (q/begin-shape)
          (doseq [[x y] (points settings)]
            (q/curve-vertex x y))
          (q/end-shape))
        "circles"
        ;; alternative, do circle packing, no-overlap?
        (dotimes [_ (/ flows-per-iter 4)]
          (doseq [p (points settings)]
            (cq/circle p hstep)))
        "triangles"
        (dotimes [_ (/ flows-per-iter 4)]
          ;; TODO: orient triangle towards flow?
          (doseq [p (points settings)]
            (cq/draw-triangle (tm/+ p (gv/vec2 hstep (- hstep)))
                              (tm/+ p (gv/vec2 (- hstep) (- hstep)))
                              (tm/+ p (gv/vec2 0 hstep)))))))))

(defn explanation []
  [:div
   [:section
    (ctrl/dropdown settings "Algorithm" [:calc-points] =
                   {"Angle from Noise" "flow-points"
                    "Flow Downhill" "downhill-points"})
    (ctrl/dropdown settings "Draw" [:draw] =
                   {"Curved Lines" "curves"
                    "Segmented Lines" "segments"
                    "Circles" "circles"
                    "Triangles" "triangles"})
    (ctrl/dropdown settings
                   "Snap Angles To " [:snap-resolution]
                   (fn [s v] (< (Math/abs (- s v)) 0.01))
                   {"Disabled" 0
                    "90 degrees" (/ Math/PI 2)
                    "60 degrees" (/ Math/PI 3)
                    "45 degrees" (/ Math/PI 4)
                    "30 degrees" (/ Math/PI 6)
                    "20 degrees" (/ Math/PI 9)
                    "15 degrees" (/ Math/PI 12)
                    "10 degrees" (/ Math/PI 18)})
    (ctrl/slider settings (fn [v] (str "Iterations " (* flows-per-iter v))) [:iterations] [1 500])
    (ctrl/slider settings (fn [v] (str "Stroke Weight " (/ 1 v))) [:stroke-weight] [1 64])
    (ctrl/slider settings (fn [v] (str "Step Size " v)) [:step-size] [1 64])
    (ctrl/slider settings (fn [v] (str "Length " v)) [:length] [8 128])
    (ctrl/slider settings (fn [v] (str "Noise Multiplier 1/" (Math/pow 2 v))) [:noise-div] [0 12])
    (ctrl/slider settings (fn [v] (if (> v 0) (str "Jitter 1/" v "* step-size")
                                     "No Jitter")) [:jitter] [0 32])]])

(defn ^:export run-sketch []
  ;; 20210617
  (ctrl/mount explanation)
  (q/defsketch flow-fields
    :host "quil-host"
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
