(ns shimmers.sketches.mechanism
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.common.framerate :as framerate]
            [shimmers.common.quil :as cq]
            [thi.ng.geom.circle :as gc]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.line :as gl]
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
  ( + diametral-pitch (* 2 (addendum gear))))

(defn whole-depth [{:keys [diametral-pitch]}]
  (/ 2.157 diametral-pitch))

(defn dedendum [gear]
  (- (whole-depth gear) (addendum gear)))

;; https://en.wikipedia.org/wiki/Gear#Spur
;; http://www.gearseds.com/files/Approx_method_draw_involute_tooth_rev2.pdf
(defn tooth [gear p]
  (let [polar (geom/as-polar p)
        thickness (tooth-thickness gear)
        pitch (/ thickness 2.5)
        addendum (addendum gear)
        dedendum (dedendum gear)]
    (mapv (fn [t] (geom/as-cartesian (tm/+ polar (gv/vec2 t))))
          [[(- dedendum) (- thickness)]
           [0 (- thickness)]
           [addendum (- pitch)]
           [addendum pitch]
           [0 thickness]
           [(- dedendum) thickness]])))

(defn poly-at [polygon pos t]
  (-> polygon
      (geom/rotate t)
      (geom/translate pos)
      geom/vertices))

(defn gear [diametral-pitch teeth]
  (let [gear {:diametral-pitch diametral-pitch :teeth teeth}
        radius (pitch-radius gear)
        points (geom/vertices (gc/circle (gv/vec2) radius) teeth)]
    (merge gear
           {:shape (gp/polygon2 (mapcat (partial tooth gear) points))
            :angle (gl/line2 (gv/vec2) (gv/vec2 (* 0.66 radius) 0))})))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0})

(defn update-state [state]
  (update state :t + 0.02))

(defn driven-by
  [gear {:keys [pos] :as driver} angle offset t]
  [gear
   (->> (gv/vec2 (center-distance driver gear) angle)
        geom/as-cartesian
        (tm/+ pos))
   (- offset (/ t (gear-ratio driver gear)))])

(defn draw [{:keys [t]}]
  (q/background 1.0)
  (let [center (gv/vec2 (cq/rel-pos 0.5 0.5))
        driver (assoc (gear 0.2 13) :pos center)
        left (gear 0.2 24)
        right (gear 0.2 52)]
    (doseq [[g pos t]
            [[driver center t]
             ;; how to solve for offset for meshing?
             (driven-by left driver Math/PI 0 t)
             (driven-by right driver 0 0.3 t)]]
      (q/stroke 0)
      (cq/draw-shape (poly-at (:shape g) pos t))
      (q/stroke 0 0.6 0.6)
      (apply q/line (poly-at (:angle g) pos t)))))

(defn ^:export run-sketch []
  ;; 20210419
  (q/defsketch mechanism
    :host "quil-host"
    :size [800 800]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode]))
