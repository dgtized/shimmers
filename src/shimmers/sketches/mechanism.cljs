(ns shimmers.sketches.mechanism
  (:require
   [loom.graph :as lg]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.model.mechanism
    :as mech
    :refer [attached-to belt driven-by gear piston pulley ring-gear wheel]]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; randomly generate gear systems that don't intersect with themselves
;; additional mechanisms like:
;;  * planatary gears (adjusts position of subystem relative to t)
;;  * piston/rod -- or at least lateral movement
;;  * flat gears?
;;  * screw gears?
;;  * pulley/belt systems?
;;  * sun & planet?
;;  * kinematic chain to another gear?
(defn gear-system [diametral-pitch driver-teeth driver-ratio]
  (let [dp diametral-pitch
        dp1 (* 0.66 dp)
        dp2 (* 1.25 dp)
        driver (assoc (gear dp driver-teeth)
                      :id 0 :dir 1 :ratio driver-ratio :offset 0)
        g (lg/add-nodes (lg/digraph) driver)
        driver-radius (mech/pitch-radius driver)
        [g driver-wheel] (attached-to g (wheel (* 0.8 driver-radius)) driver inc)
        [g dw-receiver]
        (driven-by g (wheel (* 0.4 driver-radius)) driver-wheel
                   (belt (* 3 driver-radius) (* eq/TAU 0.2)))
        [g _dw-gear] (attached-to g (gear dp 25) dw-receiver dec)
        [g left-step] (driven-by g (gear dp 20) driver (* 0.8 Math/PI))
        [g left] (attached-to g (gear dp2 70) left-step dec)
        [g left2] (driven-by g (gear dp2 16) left Math/PI)
        [g piston-driver] (driven-by g (gear dp2 26) left (/ Math/PI 2))
        [g piston-driver-b] (driven-by g (gear dp2 16) piston-driver Math/PI)
        [g piston-driver-c] (driven-by g (gear dp2 26) piston-driver-b Math/PI)
        [g _] (piston g (* 0.5 Math/PI) piston-driver)
        [g _] (piston g (* 0.5 Math/PI) piston-driver-c)
        [g _] (piston g Math/PI left2)
        [g right] (driven-by g (gear dp 20) driver 0)
        [g above] (driven-by g (gear dp 26) right (* eq/TAU 0.78))
        [g top-right] (driven-by g (gear dp 60) above (- (/ Math/PI 3)))
        [g top-right-b] (attached-to g (gear dp1 20) top-right inc)
        tr-radius (mech/pitch-radius top-right-b)
        [g wheel-driver] (attached-to g (wheel (* 0.7 tr-radius)) top-right-b inc)
        [g belt-receiver]
        (driven-by g (wheel (* 1.2 tr-radius)) wheel-driver
                   (pulley (* 3.5 tr-radius) (* 0.92 eq/TAU)))
        [g _belt-gear] (attached-to g (gear dp1 18) belt-receiver inc)
        [g tr-left] (driven-by g (gear dp1 38) top-right-b (* eq/TAU 0.56))
        [g tr-attach] (driven-by g (gear dp1 20) tr-left (* eq/TAU 0.47))
        [g tr-bottom] (attached-to g (gear dp2 80) tr-attach dec)
        [g tr-last] (driven-by g (gear dp2 35) tr-bottom (* eq/TAU 0.39))
        [g tr-step] (attached-to g (gear dp 12) tr-last inc)
        [g ring] (driven-by g (ring-gear dp 36) tr-step (* eq/TAU 0.25))
        [g below] (driven-by g (gear dp 38) right (/ Math/PI 3))
        [g _] (piston g 0 below)
        [g _] (driven-by g (gear dp 12) ring (* eq/TAU 0.25))
        [g big] (driven-by g (gear dp 128) below 0.1)
        [g s-big] (driven-by g (gear dp 20) big (* eq/TAU 0.68))
        [g _] (piston g (* eq/TAU 0.85) s-big)
        [g _] (driven-by g (gear dp 13) big (* eq/TAU 0.6))
        [g small] (driven-by g (gear dp 21) big (* 0.8 eq/TAU))
        [g small-a] (attached-to g (gear dp1 32) small dec)
        ;; FIXME: mesh-offset is wrong for rings on: non-cardinal directions,
        ;; some diametral-pitch, and certain teeth counts
        [g _] (driven-by g (ring-gear dp1 46) small-a 0)]
    g))

(comment (-> (gear-system 0.3 30 1.0)
             (mech/propagate-position (gv/vec2) 0)))

(defn ring-test [diametral-pitch driver-teeth driver-ratio]
  (let [dp diametral-pitch
        dp-a (* 1.1 dp)
        dp-b (* 0.75 dp)
        driver (assoc (gear dp driver-teeth)
                      :id 0 :dir 1 :ratio driver-ratio :offset 0)
        g (lg/add-nodes (lg/digraph) driver)
        [g ring1] (driven-by g (ring-gear dp (int (* 2.1 driver-teeth))) driver 0)
        [g _] (driven-by g (gear dp (int (* 0.4 driver-teeth))) ring1 (* eq/TAU 0.7))
        [g _] (driven-by g (gear dp (int (* 0.6 driver-teeth))) ring1 (* eq/TAU 0.13))
        [g _] (driven-by g (gear dp (int (* 0.5 driver-teeth))) ring1 (* eq/TAU 0))
        [g _] (driven-by g (gear dp (int (* 0.3 driver-teeth))) ring1 (* eq/TAU 0.25))
        [g in-driver2] (attached-to g (gear dp-a (int (* 0.7 driver-teeth))) driver inc)
        [g ring2] (driven-by g (ring-gear dp-a 120) in-driver2 (* eq/TAU 0.66))
        [g _] (driven-by g (gear dp-a 18) ring2 (* eq/TAU 0.45))
        [g _] (driven-by g (gear dp-a 19) ring2 (* eq/TAU 0.55))
        [g _] (driven-by g (gear dp-a 20) ring2 (* eq/TAU 0.66))
        [g _] (driven-by g (gear dp-a 21) ring2 (* eq/TAU 0.76))
        [g _] (driven-by g (gear dp-a 22) ring2 (* eq/TAU 0.87))
        [g in-driver] (attached-to g (gear dp-b (int (* 0.33 driver-teeth))) driver (partial + 2))
        [g ring3] (driven-by g (ring-gear dp-b 60) in-driver (* eq/TAU 0.40))
        [g _] (driven-by g (gear dp-b 11) ring3 (* eq/TAU 0.65))
        [g _] (driven-by g (gear dp-b 15) ring3 (* eq/TAU 0.5))
        [g _] (driven-by g (gear dp-b 18) ring3 (* eq/TAU 0.35))]
    g))

(defonce defo (debug/state {}))

;; Visualization & User Interface
(defonce ui-state
  (ctrl/state {:mode :gears
               :running true
               :show-angle-path false
               :diametral-pitch 0.35
               :driver-teeth 42
               :driver-ratio 1.0}))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:mouse (gv/vec2)
   :t 0})

(defn update-state [state]
  (-> (if (:running @ui-state)
        (update state :t + 0.01)
        state)
      (update :mouse cq/mouse-last-position-clicked)))

(defn draw-gear [sys {:keys [radius] :as gear} t]
  (let [theta (mech/rotation gear t)
        pos (mech/position sys gear)]
    (q/fill 1.0)
    (cq/draw-shape (mech/gear-polygon gear pos theta))
    (when (:show-angle-path @ui-state)
      (q/with-stroke [0 0.6 0.6]
        (q/line pos (tm/+ pos (v/polar (* 0.66 radius) theta)))))))

(defn draw-ring-gear [sys {:keys [radius teeth] :as gear} selected? t]
  (let [theta (mech/rotation gear t)
        pos (mech/position sys gear)
        outer-r (+ radius (* 3 (mech/addendum gear)))]
    (q/fill 1.0)
    (q/begin-shape)
    (doseq [t (range teeth)
            :let [v (v/polar outer-r (+ theta (* (/ t teeth) eq/TAU)))
                  [x y] (tm/+ pos v)]]
      (q/vertex x y))
    (q/begin-contour)
    ;; reverse points to counter-clockwise ordering, as contour subtraction
    ;; requires that inner polygon has opposing winding order of outer polygon.
    (doseq [[x y] (reverse (mech/gear-polygon gear pos theta))]
      (q/vertex x y))
    (q/end-contour)
    (q/end-shape :close)
    (when (:show-angle-path @ui-state)
      (q/with-stroke [0 0.6 0.6]
        (q/line pos (tm/+ pos (v/polar (* 0.66 radius) theta)))))
    (when selected?
      (let [{:keys [angle]} gear
            r (+ radius (* 2 (mech/addendum gear)))
            high-pt (v/polar r (- angle Math/PI))
            low-pt (v/polar r (- angle Math/PI (:offset gear)))]
        (q/with-stroke [0.33 0.5 0.4]
          (cq/circle (tm/+ pos high-pt) 3))
        (q/with-stroke [0 0.6 0.6]
          (cq/circle (tm/+ pos low-pt) 3))))))

;; TODO: correct attach-radius for ring-gear so it's outside of radius
(defn draw-piston [sys {:keys [angle] :as part} t]
  (let [{:keys [radius] :as driver} (mech/driver sys part)
        pos (mech/position sys driver)
        inner (* 2.5 (mech/dedendum driver))
        attach-radius (- radius inner)
        connecting-len (* 2.1 radius)
        piston-len (* 1.8 inner)

        theta (mech/rotation driver t)
        attached-pt (tm/+ pos (v/polar attach-radius theta))
        displacement (mech/piston-displacement attach-radius connecting-len (- theta angle))
        socket-pt (tm/+ pos (v/polar displacement angle))]
    (cq/draw-shape (cq/box-line attached-pt socket-pt (* 0.2 inner)))
    (cq/circle attached-pt (* 0.3 inner))
    (cq/draw-shape (cq/box-line socket-pt (tm/+ socket-pt (v/polar piston-len angle)) (* 0.8 inner)))
    (cq/circle socket-pt (* 0.3 inner))

    (when (:show-angle-path @ui-state)
      (q/with-stroke [0 0.6 0.6]
        (q/line (tm/+ pos (v/polar (- connecting-len attach-radius) angle))
                (tm/+ pos (v/polar (+ connecting-len attach-radius) angle)))))))

(defn draw-wheel [sys {:keys [radius distance] :as wheel} t]
  (let [pos (mech/position sys wheel)
        driver (mech/driver sys wheel)]
    (cq/circle pos radius)
    (when (= (:type driver) :wheel)
      (let [driver-pos (mech/position sys driver)
            driver-radius (:radius driver)
            wheel-heading (g/heading (tm/- driver-pos pos))]
        (case (mech/drive sys driver wheel)
          :belt
          (let [phi (mech/belt-phi radius driver-radius distance)
                driver-heading (+ Math/PI wheel-heading)]
            (q/line (tm/+ driver-pos (v/polar driver-radius (- driver-heading phi)))
                    (tm/+ pos (v/polar radius (- wheel-heading phi))))
            (q/line (tm/+ driver-pos (v/polar driver-radius (+ driver-heading phi)))
                    (tm/+ pos (v/polar radius (+ wheel-heading phi)))))
          :pulley
          (let [phi (mech/pulley-phi radius driver-radius distance)
                driver-heading wheel-heading]
            (q/line (tm/+ driver-pos (v/polar driver-radius (- driver-heading phi)))
                    (tm/+ pos (v/polar radius (- wheel-heading phi))))
            (q/line (tm/+ driver-pos (v/polar driver-radius (+ driver-heading phi)))
                    (tm/+ pos (v/polar radius (+ wheel-heading phi)))))
          )))))

(defn draw-part [sys {:keys [type] :as part} selected-ids t]
  (q/stroke 0)
  (q/stroke-weight 1.0)
  (let [selected? (contains? selected-ids (:id part))]
    (when selected?
      (q/stroke 0.55 0.6 0.3)
      (q/stroke-weight 1.5))
    (case type
      :gear (draw-gear sys part t)
      :ring-gear (draw-ring-gear sys part selected? t)
      :piston (draw-piston sys part t)
      :wheel (draw-wheel sys part t))))

(def system-modes [:gears :ring-test])
(defn system-mode [mode]
  (case mode
    :ring-test [ring-test (cq/rel-vec 0.5 0.55)]
    :gears [gear-system (cq/rel-vec 0.32 0.51)]))

(defn selected-parts [sys parts mouse]
  (let [close-parts
        (->> parts
             (map (fn [p] (assoc p :pos (mech/position sys p))))
             (sort-by (fn [{:keys [pos]}] (g/dist-squared pos mouse))))]
    (if-let [closest (first (filter :radius close-parts))]
      (if (< (g/dist (:pos closest) mouse) (:radius closest))
        (take-while (fn [{:keys [pos]}] (= (:pos closest) pos)) close-parts)
        [])
      [])))

;; Add stroke shading along the teeth somehow?
;; Add inner shapes like N spokes or crankshaft hole?
(defn draw [{:keys [t mouse]}]
  (q/ellipse-mode :radius)
  (q/no-fill)
  (q/background 1.0)
  (let [{:keys [mode diametral-pitch driver-teeth driver-ratio]} @ui-state
        [system-graph origin] (system-mode mode)
        sys (-> (system-graph diametral-pitch driver-teeth driver-ratio)
                (mech/propagate-position origin t))
        parts (mech/components sys)
        selected (selected-parts sys parts mouse)
        selected-ids (set (map :id selected))]
    (swap! defo assoc :parts selected)
    (doseq [part (sort-by :depth parts)]
      (draw-part sys part selected-ids t))))

(defn ui-controls []
  [:div.flexcols
   [:div {:style {:width "20em"}}
    (ctrl/change-mode ui-state system-modes)
    (ctrl/checkbox ui-state "Running?" [:running])
    (ctrl/checkbox ui-state "Show Angle/Path" [:show-angle-path])
    (ctrl/numeric ui-state "Diametral Pitch" [:diametral-pitch] [0.05 1.0 0.01])
    (ctrl/numeric ui-state "Driver Teeth" [:driver-teeth] [10 64 1])
    (ctrl/numeric ui-state "Driver Ratio" [:driver-ratio] [0.5 4.0 0.1])]
   [:div (debug/display defo)]])

(sketch/defquil mechanism
  :created-at "2021-04-19"
  :size [900 600]
  :on-mount #(ctrl/mount ui-controls)
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
