(ns shimmers.sketches.mechanism
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
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
(defn tooth [gear p]
  (let [polar (g/as-polar p)
        thickness (tooth-thickness gear)
        pitch (/ thickness 2.5)
        addendum (addendum gear)
        dedendum (dedendum gear)]
    (mapv (fn [t] (g/as-cartesian (tm/+ polar (gv/vec2 t))))
          [[(- dedendum) (- thickness)]
           [0 (- thickness)]
           [addendum (- pitch)]
           [addendum pitch]
           [0 thickness]
           [(- dedendum) thickness]])))

(defn poly-at [polygon pos t]
  (-> polygon
      (g/rotate t)
      (g/translate pos)
      g/vertices))

(defn gear [diametral-pitch teeth]
  (let [gear {:diametral-pitch diametral-pitch :teeth teeth}
        radius (pitch-radius gear)
        points (g/vertices (gc/circle (gv/vec2) radius) teeth)]
    (merge gear
           {:radius radius
            :shape (gp/polygon2 (mapcat (partial tooth gear) points))})))


;; https://stackoverflow.com/questions/13456603/calculate-offset-rotation-to-allow-gears-to-mesh-correctly/17381710
;; and http://kirox.de/html/Gears.html (GearView.setPos)
(defn meshing-interlock-angle
  "Calculate the initial angle for meshing with `driver` gear.

  `angle` is the heading of the vector between the driving gear and the connecting gear."
  [{:keys [teeth] :as gear}
   {:keys [offset dir] :as driver}
   angle]
  (if driver
    ;; mod by (/ tm/TWO_PI teeth) to keep this small
    (let [gear-ratio (gear-ratio gear driver)]
      (+ (* gear-ratio offset)
         (* (+ 1 gear-ratio) (* -1 dir angle))
         (* (mod (inc teeth) 2) (/ Math/PI teeth)) ;; add a tooth width if even?
         ))
    0))

(defn driven-by
  [gear
   {:keys [pos dir ratio] :as driver} angle]
  (assoc gear
         :pos (tm/+ pos (v/polar (center-distance driver gear) angle))
         :dir (* -1 dir)
         :ratio (* ratio (gear-ratio driver gear))
         :offset (meshing-interlock-angle gear driver angle)))

;; randomly generate gear systems that don't intersect with themselves
;; additional mechanisms like:
;;  * planatary gears (adjusts position of subystem relative to t)
;;  * piston/rod -- or at least lateral movement
;;  * flat gears?
;;  * screw gears?
;;  * pulley/belt systems?
;;  * sun & planet?
;;  * kinematic chain to another gear?
(defn gear-system [center diametral-pitch driver-teeth driver-ratio]
  (let [dp diametral-pitch
        driver (assoc (gear dp driver-teeth) :pos center :dir 1 :ratio driver-ratio :offset 0)
        left (driven-by (gear dp 40) driver Math/PI)
        right (driven-by (gear dp 25) driver 0)
        above (driven-by (gear dp 21) right (- (/ Math/PI 2)))
        below (driven-by (gear dp 30) right (/ Math/PI 2))]
    [driver left
     (driven-by (gear dp 20) left (/ Math/PI 2))
     (driven-by (gear dp 12) left Math/PI)
     right above
     (driven-by (gear dp 50) above (- (/ Math/PI 3)))
     below
     (driven-by (gear dp 8) right -0.5)
     (driven-by (gear dp 128) below (/ Math/PI 3))]))

(defn rotation [{:keys [dir ratio offset]} t]
  (* dir (+ (/ t ratio) offset)))

(comment (map #(dissoc % :shape) (gear-system (gv/vec2) 0.3 30 1.0)))

;; Visualization & User Interface
(defonce ui-state
  (ctrl/state {:running true
               :diametral-pitch 0.25
               :driver-teeth 30
               :driver-ratio 1.0}))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0})

(defn update-state [state]
  (if (:running @ui-state)
    (update state :t + 0.01)
    state))

;; Add stroke shading along the teeth somehow?
;; Add inner shapes like N spokes or crankshaft hole?
(defn draw [{:keys [t]}]
  (q/ellipse-mode :radius)
  (q/no-fill)
  (q/background 1.0)
  (let [{:keys [diametral-pitch driver-teeth driver-ratio]} @ui-state]
    (doseq [{:keys [shape radius pos] :as gear}
            (gear-system (cq/rel-vec 0.5 0.5) diametral-pitch driver-teeth driver-ratio)
            :let [theta (rotation gear t)]]
      (q/stroke-weight 1.0)
      (q/stroke 0)
      (cq/draw-shape (poly-at shape pos theta))
      (q/stroke 0 0.6 0.6)
      (q/line pos (tm/+ pos (v/polar (* 0.66 radius) theta))))))

(defn ui-controls []
  [:div {:style {:width "20em"}}
   (ctrl/checkbox ui-state "Running?" [:running])
   (ctrl/numeric ui-state "Diametral Pitch" [:diametral-pitch] [0.05 1.0 0.01])
   (ctrl/numeric ui-state "Driver Teeth" [:driver-teeth] [10 64 1])
   (ctrl/numeric ui-state "Driver Ratio" [:driver-ratio] [0.5 4.0 0.1])])

(sketch/defquil mechanism
  :created-at "2021-04-19"
  :size [800 800]
  :on-mount #(ctrl/mount ui-controls)
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
