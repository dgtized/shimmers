(ns shimmers.sketches.periapsis
  (:require
   [kixi.stats.distribution :as ksd]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; Math cobbled together from:
;; https://en.wikipedia.org/wiki/Ellipse
;; https://en.wikipedia.org/wiki/Orbit_equation
;; https://en.wikipedia.org/wiki/Elliptic_orbit
(defn semi-minor [eccentricity semi-major]
  (* semi-major (Math/sqrt (- 1.0 (Math/pow eccentricity 2)))))

;; TODO: Consider making speed proportional to mass and semi-major to match
;; escape velocities correctly?
(defrecord Body [mass semi-major semi-minor focal-distance rotation speed theta0 moons])

;; TODO: Fix offsets somehow so children cannot clip parents they orbit?
;; FIXME: velocity is fixed and not proportional to distance to parent?
(defn position [{:keys [semi-major semi-minor focal-distance
                        speed theta0]} t]
  (let [dt (if (> (abs speed) 0) speed 1)
        theta (+ theta0 (/ t dt))]
    (gv/vec2 (+ focal-distance (* semi-major (Math/cos theta)))
             (* semi-minor (Math/sin theta)))))

(defn orbital-period [semi-major mass]
  (let [G 30000.0]
    (* (Math/sqrt (/ (Math/pow semi-major 3)
                     (* G mass)))
       eq/TAU)))

(defn make-body [{:keys [semi-major eccentricity direction mass] :as params
                  :or {semi-major 0
                       eccentricity 0
                       direction 1}}]
  (map->Body (merge {:semi-major semi-major
                     :semi-minor (semi-minor eccentricity semi-major)
                     :eccentricity eccentricity
                     :focal-distance (* eccentricity semi-major)
                     :speed (* direction (orbital-period semi-major mass))
                     :theta0 (dr/random-tau)
                     :rotation (dr/random-tau)
                     :moons []}
                    params)))

(defn make-moon []
  (make-body {:semi-major (dr/random 8 24)
              :eccentricity (dr/happensity 0.3)
              :mass (/ (dr/random 1 4) 4)
              :direction (if (dr/chance 0.1) -1 1)}))

(defn make-bodies [n]
  (let [radius (/ (q/width) 2)
        base 8
        base-radius (* base 4)]
    (cons
     (make-body {:mass (* base 2)})
     (repeatedly
      n
      #(let [semi-major (+ base-radius (* (- radius base-radius) (Math/sqrt (dr/random))))]
         (make-body
          {:semi-major semi-major
           ;; eccentricity likelyhood is proportional to to size of orbit for aesthetics
           :eccentricity (dr/happensity (* 0.75 (tm/smoothstep* (/ radius 10) (/ radius 1.5) semi-major)))
           :mass (/ (dr/random 8 12) 4)
           :direction (if (dr/chance 0.1) -1 1)
           :moons (repeatedly (ksd/draw (ksd/poisson {:lambda 0.9})
                                        {:seed (dr/random-int dr/MAX-INT)})
                              make-moon)}))))))

(comment
  (->> (ksd/poisson {:lambda 0.9})
       (ksd/sample 192)
       frequencies))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:bodies (make-bodies 192)
   :t 0.0})

(defn update-state [state]
  (update state :t + 0.1))

(defn draw [{:keys [bodies t]}]
  (q/ellipse-mode :radius)
  (q/background 1.0 0.25)
  (q/no-stroke)
  (q/fill 0.0 0.7)
  (q/with-translation (cq/rel-pos 0.5 0.5)
    (doseq [{:keys [mass moons rotation] :as body} bodies
            :let [pos (position body t)]]
      (q/with-rotation [rotation]
        (cq/circle pos mass)
        (q/with-translation pos
          (doseq [{:keys [mass rotation] :as moon} moons]
            (q/with-rotation [rotation]
              (cq/circle (position moon t) mass))))))))

(defn page []
  (sketch/component
   :size [800 800]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition periapsis
  {:created-at "2021-07-06"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
