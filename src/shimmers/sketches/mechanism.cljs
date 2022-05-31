(ns shimmers.sketches.mechanism
  (:require
   [loom.alg :as la]
   [loom.attr :as lga]
   [loom.graph :as lg]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; useful formula
;; https://www.engineersedge.com/gear_formula.htm
;; https://www.cs.cmu.edu/~rapidproto/mechanisms/chpt7.html
;; https://www.bostongear.com/-/media/Files/Literature/Brand/boston-gear/catalogs/p-1930-bg-sections/p-1930-bg_engineering-info-spur-gears.ashx

;; Teeth Count; N = P * D
;; Diametral Pitch; P = π / p or N/D
;; Pitch Diameter; D = N/P or D0 - 2/P
;; Tooth Thickness; t = π / (2P)
;; Addendum; a = 1/P
;; Outside Diameter; D0 = D + 2a
;; Whole Depth; h_t = 2.2/P + .002 or 2.157/P
;; Dedendum; b = h_t - a

(defn pitch-diameter [{:keys [teeth diametral-pitch]}]
  (/ teeth diametral-pitch))

(defn pitch-radius [gear]
  (/ (pitch-diameter gear) 2))

;; https://blog.misumiusa.com/center-to-center-spacing-for-shafts-spur-gears/
(defn center-distance [gear1 gear2]
  (* 0.5 (+ (pitch-diameter gear1) (pitch-diameter gear2))))

(defn ring-center-distance [gear ring]
  (* 0.5 (- (pitch-diameter ring) (pitch-diameter gear))))

(defn gear-ratio [gear-in gear-out]
  (/ (:teeth gear-out) (:teeth gear-in)))

;; in radians
(defn tooth-thickness [{:keys [teeth]}]
  (/ Math/PI (* 2 teeth)))

(defn addendum [{:keys [diametral-pitch]}]
  (/ 1 diametral-pitch))

(defn outside-diameter [{:keys [diametral-pitch] :as gear}]
  (+ diametral-pitch (* 2 (addendum gear))))

(defn whole-depth [{:keys [diametral-pitch]}]
  (/ 2.157 diametral-pitch))

(defn dedendum [gear]
  (- (whole-depth gear) (addendum gear)))

;; https://en.wikipedia.org/wiki/Gear#Spur
;; http://www.gearseds.com/files/Approx_method_draw_involute_tooth_rev2.pdf
(defn involute-tooth [{:keys [type] :as gear}]
  (let [thickness (tooth-thickness gear)
        pitch (/ thickness 2.5)
        invert (if (= type :ring-gear) -1 1)
        addendum (* invert (addendum gear))
        dedendum (* invert (dedendum gear))]
    [[(- dedendum) (- thickness)]
     [0 (- thickness)]
     [addendum (- pitch)]
     [addendum pitch]
     [0 thickness]
     [(- dedendum) thickness]]))

(defn gear-polygon [{:keys [radius teeth] :as gear} pos theta]
  (let [tooth (involute-tooth gear)]
    (->> (for [v (range teeth)]
           (let [t (+ (* eq/TAU (/ v teeth)) theta)]
             (map (fn [[dr dt]]
                    (->> (gv/vec2 (+ radius dr) (+ t dt))
                         g/as-cartesian
                         (tm/+ pos)))
                  tooth)))
         (apply concat))))

(defn gear [diametral-pitch teeth]
  (let [gear {:depth 0
              :type :gear
              :diametral-pitch diametral-pitch
              :teeth teeth}]
    (assoc gear :radius (pitch-radius gear))))

(defn ring-gear [diametral-pitch teeth]
  (let [gear {:depth 0
              :type :ring-gear
              :diametral-pitch diametral-pitch
              :teeth teeth}]
    (assoc gear :radius (pitch-radius gear))))

;; https://stackoverflow.com/questions/13456603/calculate-offset-rotation-to-allow-gears-to-mesh-correctly/17381710
;; and http://kirox.de/html/Gears.html (GearView.setPos)
(defn meshing-interlock-angle
  "Calculate the initial angle for meshing with `driver` gear.

  `angle` is the heading of the vector between the driving gear and the connecting gear."
  [{:keys [teeth dir] :as gear}
   {:keys [offset] :as driver}
   angle]
  (if driver
    (let [gear-ratio (gear-ratio gear driver)]
      (-> (* gear-ratio offset)
          (+ (* (+ 1 gear-ratio) (* dir angle)))
          (+ (if (= :ring-gear (:type gear))
               0 ;; for angle 0
               ;; (* angle 0.004) ;; for angle (* eq/TAU 0.40)
               ;; (* angle 0.0025) ;; for angle (* eq/TAU 0.66)
               0))
          ;; add a tooth width if even?
          (+ (* (mod (inc teeth) 2) (/ Math/PI teeth)))
          (mod (/ tm/TWO_PI teeth))))
    0))

(defn ring-gear-mesh? [gear driver]
  (or (= (:type gear) :ring-gear)
      (= (:type driver) :ring-gear)))

(defn add-part [sys part driver]
  [(-> sys
       (lg/add-nodes part)
       (lg/add-edges [driver part]))
   part])

(defn driven-by
  [sys
   {gear-type :type :as gear}
   {driver-type :type :keys [dir ratio depth] :as driver} angle]
  {:pre [(= (:diametral-pitch gear) (:diametral-pitch driver))
         (contains? #{[:gear :gear]
                      [:ring-gear :gear]
                      [:gear :ring-gear]}
                    [gear-type driver-type])]}
  (let [angle' (if (= driver-type :ring-gear) (+ angle Math/PI) angle)
        gear' (assoc gear
                     :id (count (lg/nodes sys))
                     :depth depth
                     :angle angle'
                     :dir (if (ring-gear-mesh? gear driver) dir (* -1 dir))
                     :ratio (* ratio (gear-ratio driver gear)))
        gear' (assoc gear' :offset (meshing-interlock-angle gear' driver angle'))]
    (add-part sys gear' driver)))

(defn attached-to
  [sys gear {:keys [depth dir ratio offset] :as driver} depth-dir]
  (let [gear' (assoc gear
                     :id (count (lg/nodes sys))
                     :depth (depth-dir depth)
                     :dir dir
                     :ratio ratio
                     :offset offset ;; or 0
                     )]
    (add-part sys gear' driver)))

(defn piston [sys angle driver]
  (let [piston {:id (count (lg/nodes sys))
                :type :piston
                :depth (inc (:depth driver))
                :angle angle}]
    (add-part sys piston driver)))

(defn piston-displacement
  "Calculates displacement along the axis of a piston from `theta` of the circle.

  From https://en.wikipedia.org/wiki/Piston_motion_equations#Deriving_angle_domain_equations"
  [radius length theta]
  (+ (* radius (Math/cos theta))
     (Math/sqrt (- (eq/sqr length)
                   (* (eq/sqr radius)
                      (eq/sqr (Math/sin theta)))))))

(defn driver [sys part]
  (let [preds (lg/predecessors sys part)]
    (assert (<= (count preds) 1)
            "part should have at most 1 driver")
    (first preds)))

(defn rotation [{:keys [dir ratio offset]} t]
  (* dir (+ (/ t ratio) offset)))

(defn propagate-position [system origin _]
  (reduce (fn [sys {part-type :type :keys [angle] :as part}]
            (if-let [driver (driver sys part)]
              (let [pos (lga/attr sys driver :pos)]
                (if (and angle (not= part-type :piston))
                  (lga/add-attr sys part :pos
                                (tm/+ pos
                                      (if (ring-gear-mesh? part driver)
                                        (v/polar (ring-center-distance driver part) angle)
                                        (v/polar (center-distance driver part) angle))))
                  (lga/add-attr sys part :pos pos)))
              (lga/add-attr sys part :pos origin)))
          system
          (la/topsort system)))

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
             (propagate-position (gv/vec2) 0)))

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
        [g _] (driven-by g (gear dp-b 18) ring3 (* eq/TAU 0.35))
]
    g))

(defonce defo (debug/state {}))

;; Visualization & User Interface
(defonce ui-state
  (ctrl/state {:mode :gears
               :running true
               :show-angle-path true
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
  (let [theta (rotation gear t)
        pos (lga/attr sys gear :pos)]
    (q/fill 1.0)
    (cq/draw-shape (gear-polygon gear pos theta))
    (when (:show-angle-path @ui-state)
      (q/with-stroke [0 0.6 0.6]
        (q/line pos (tm/+ pos (v/polar (* 0.66 radius) theta)))))))

(defn draw-ring-gear [sys {:keys [radius teeth] :as gear} t]
  (let [theta (rotation gear t)
        pos (lga/attr sys gear :pos)
        outer-r (+ radius (* 3 (addendum gear)))]
    (q/fill 1.0)
    (q/begin-shape)
    (doseq [t (range teeth)
            :let [v (v/polar outer-r (+ theta (* (/ t teeth) eq/TAU)))
                  [x y] (tm/+ pos v)]]
      (q/vertex x y))
    (q/begin-contour)
    ;; reverse points to counter-clockwise ordering, as contour subtraction
    ;; requires that inner polygon has opposing winding order of outer polygon.
    (doseq [[x y] (reverse (gear-polygon gear pos theta))]
      (q/vertex x y))
    (q/end-contour)
    (q/end-shape :close)
    (when (:show-angle-path @ui-state)
      (q/with-stroke [0 0.6 0.6]
        (q/line pos (tm/+ pos (v/polar (* 0.66 radius) theta)))))))

;; TODO: correct attach-radius for ring-gear so it's outside of radius
(defn draw-piston [sys {:keys [angle] :as part} t]
  (let [{:keys [radius] :as driver} (driver sys part)
        pos (lga/attr sys driver :pos)
        inner (* 2.5 (dedendum driver))
        attach-radius (- radius inner)
        connecting-len (* 2.1 radius)
        piston-len (* 1.8 inner)

        theta (rotation driver t)
        closest (v/polar (- connecting-len attach-radius) angle)
        furthest (v/polar (+ connecting-len attach-radius) angle)
        attached-pt (tm/+ pos (v/polar attach-radius theta))
        displacement (piston-displacement attach-radius connecting-len (- theta angle))
        socket-pt (tm/+ pos (v/polar displacement angle))]
    (cq/draw-shape (cq/box-line attached-pt socket-pt (* 0.2 inner)))
    (cq/circle attached-pt (* 0.3 inner))
    (cq/draw-shape (cq/box-line socket-pt (tm/+ socket-pt (v/polar piston-len angle)) (* 0.8 inner)))
    (cq/circle socket-pt (* 0.3 inner))

    (when (:show-angle-path @ui-state)
      (q/with-stroke [0 0.6 0.6]
        (q/line (tm/+ pos closest) (tm/+ pos furthest))))))

(defn draw-part [sys {:keys [type] :as part} selected-ids t]
  (q/stroke 0)
  (q/stroke-weight 1.0)
  (when (contains? selected-ids (:id part))
    (q/stroke 0.55 0.6 0.3)
    (q/stroke-weight 1.5))
  (case type
    :gear (draw-gear sys part t)
    :ring-gear (draw-ring-gear sys part t)
    :piston (draw-piston sys part t)))

(def system-modes [:gears :ring-test])
(defn system-mode [mode]
  (case mode
    :ring-test [ring-test (cq/rel-vec 0.5 0.55)]
    :gears [gear-system (cq/rel-vec 0.32 0.51)]))

(defn selected-parts [sys parts mouse]
  (let [close-parts
        (->> parts
             (map (fn [p] (assoc p :pos (lga/attr sys p :pos))))
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
                (propagate-position origin t))
        parts (lg/nodes sys)
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
