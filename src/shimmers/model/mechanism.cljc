(ns shimmers.model.mechanism
  (:require
   [loom.alg :as la]
   [loom.attr :as lga]
   [loom.graph :as lg]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
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
    (sequence
     (mapcat (fn [v]
               (let [t (+ (* eq/TAU (/ v teeth)) theta)]
                 (map (fn [[dr dt]]
                        (->> (gv/vec2 (+ radius dr) (+ t dt))
                             g/as-cartesian
                             (tm/+ pos)))
                      tooth))))
     (range teeth))))

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

(defn ring-gear-mesh? [gear driver]
  (or (= (:type gear) :ring-gear)
      (= (:type driver) :ring-gear)))

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


(defn add-part [sys part driver]
  [(-> sys
       (lg/add-nodes part)
       (lg/add-edges [driver part]))
   part])

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

;; https://en.wikipedia.org/wiki/Belt_problem
(defn belt-phi [radius1 radius2 center-distance]
  (Math/acos (/ (+ radius1 radius2) center-distance)))

(defn pulley-phi [radius1 radius2 center-distance]
  (Math/acos (* 2 (/ (Math/abs (- radius1 radius2)) center-distance))))

(defn belt-ratio [wheel driver]
  (/ (:radius wheel) (:radius driver)))

(defn wheel [radius]
  {:depth 0
   :type :wheel
   :radius radius})

(defn belt [distance angle]
  {:drive :belt :angle angle :distance distance})

(defn pulley [distance angle]
  {:drive :pulley :angle angle :distance distance})

(defn driven-by
  [sys
   {gear-type :type :as gear}
   {driver-type :type :keys [dir ratio depth] :as driver}
   angle-or-by]

  {:pre
   [(= (:diametral-pitch gear) (:diametral-pitch driver))
    (contains? #{[:gear :gear]
                 [:ring-gear :gear]
                 [:gear :ring-gear]
                 [:wheel :wheel]}
               [gear-type driver-type])
    (or (number? angle-or-by)
        (every? #(contains? angle-or-by %) [:drive :angle :distance]))]}

  (if (number? angle-or-by)
    (let [angle' (if (= driver-type :ring-gear) (+ angle-or-by Math/PI) angle-or-by)
          gear' (assoc gear
                       :id (count (lg/nodes sys))
                       :depth depth
                       :angle angle'
                       :dir (if (ring-gear-mesh? gear driver) dir (* -1 dir))
                       :ratio (* ratio (gear-ratio driver gear)))
          gear' (assoc gear' :offset (meshing-interlock-angle gear' driver angle'))]
      (add-part sys gear' driver))
    (let [wheel gear
          {:keys [drive angle distance]} angle-or-by
          wheel' (assoc wheel
                        :id (count (lg/nodes sys))
                        :depth depth
                        :angle angle
                        :distance distance
                        :dir (if (= drive :belt) (* -1 dir) dir)
                        :ratio (* ratio (belt-ratio wheel driver))
                        :offset 0)]
      [(-> sys
           (lg/add-nodes wheel')
           (lg/add-edges [driver wheel'])
           (lga/add-attr-to-edges :drive drive [[driver wheel']]))
       wheel'])))

(defn driver [sys part]
  (let [preds (lg/predecessors sys part)]
    (assert (<= (count preds) 1)
            "part should have at most 1 driver")
    (first preds)))

(defn rotation [{:keys [dir ratio offset]} t]
  (* dir (+ (/ t ratio) offset)))

(defn propagate-position [system origin _]
  (reduce (fn [sys {part-type :type :keys [angle distance] :as part}]
            (if-let [driver (driver sys part)]
              (let [pos (lga/attr sys driver :pos)]
                (cond (and angle distance)
                      (lga/add-attr sys part :pos
                                    (tm/+ pos (v/polar distance angle)))

                      (and angle (not= part-type :piston))
                      (lga/add-attr sys part :pos
                                    (tm/+ pos
                                          (if (ring-gear-mesh? part driver)
                                            (v/polar (ring-center-distance driver part) angle)
                                            (v/polar (center-distance driver part) angle))))
                      :else
                      (lga/add-attr sys part :pos pos)))
              (lga/add-attr sys part :pos origin)))
          system
          (la/topsort system)))
