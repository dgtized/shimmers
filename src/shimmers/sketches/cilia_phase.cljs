(ns shimmers.sketches.cilia-phase
  (:require
   [clojure.math :as math]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; Concept: generate a random phase-shifted function, and then add cilia along
;; it's path. Specifically interested in trying to make another phase shifted
;; function and slowly modulate that over each cilia from left to right.

(def width 900)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn spline-fx []
  (let [rx (dr/weighted {1.0 1.0 0.5 1.0 2.0 1.0})
        rx' (dr/weighted {1.0 1.0 0.5 1.0 2.0 1.0})
        amp (dr/weighted {0.1 1.0 0.2 1.0 0.3 1.0 0.4 1.0})
        cx (dr/random)
        cx' (dr/random)]
    (fn [x] (math/sin (* eq/TAU (+ (* rx x) cx (* amp (math/cos (* eq/TAU (+ (* rx' x) cx'))))))))))

(defn cilia-spline-fx []
  (let [rx (dr/weighted {1.0 1.0 0.5 1.0 2.0 1.0})
        rx' (dr/weighted {1.0 1.0 0.5 1.0 2.0 1.0})
        amp (dr/weighted {0.1 1.0 0.2 1.0 0.3 1.0 0.4 1.0})
        cx (dr/random)]
    {:params {:rx rx :rx' rx' :amp amp :cx cx}
     :fn
     (fn [cx' x] (math/sin (* eq/TAU (+ (* rx x) cx (* amp (math/cos (* eq/TAU (+ (* rx' x) cx'))))))))}))

(defn screen-space [y amp fx x phase]
  (rv x (+ y (* amp (fx (+ x phase))))))

(defn base-spline [screen-space fx phase]
  (for [x (range 0 1 0.0025)]
    (screen-space fx x phase)))

(defn cilia-line [_ pt angle len]
  (let [offset (g/rotate (gv/vec2 len 0) angle)]
    (gl/line2 (tm/+ pt offset) (tm/- pt offset))))

(defn cilia-line-plot [spline cx pt angle len]
  (let [offset (g/rotate (gv/vec2 len 0) angle)
        axis (g/rotate (gv/vec2 (* 0.075 len) 0) (+ angle (* eq/TAU 0.25)))
        a (tm/+ pt offset)
        b (tm/- pt offset)]
    (csvg/path
     (csvg/segmented-path
      (for [x (range 0 1 0.02)]
        (let [fy (spline cx x)]
          (tm/+ (tm/mix a b x)
                (tm/mix (tm/- axis) axis fy))))))))

;; How to avoid intersecting cilia?
(defn cilias [screen-space cilia-spline fx spx c-amp theta-x phase]
  (for [x (range -0.05 1.05 0.005)]
    (let [pt (screen-space fx x phase)
          pt' (screen-space fx (+ x 0.0001) phase)
          rotation (* 0.125 math/PI (theta-x x))
          angle (+ (g/heading (tm/- pt' pt)) (* eq/TAU 0.25) rotation)
          len (* height (+ c-amp (* 0.75 c-amp (spx x))))]
      (cilia-spline x pt angle len))))

(defn parameters []
  (let [n (dr/weighted {(dr/random-int 3 8) 3.0
                        (dr/random-int 2 14) 1.5
                        (dr/random-int 2 20) 1.0})
        s (dr/random 1.0)
        amp (tm/clamp (dr/gaussian 0.35 0.05) 0.075 0.6)]
    (into []
          (for [y (cs/midsection (tm/norm-range n))]
            {:ry y
             :amp (* (/ 1.0 (inc n))
                     (+ 0.025 (eq/gaussian s 0.5 -0.4 y)))
             :c-amp (* (/ 1.0 (inc n))
                       (+ 0.025 (eq/gaussian amp 0.5 -0.125 y)))
             :phase (dr/gaussian 0.0 0.01)}))))

(defonce defo (debug/state))

(defn shapes [{:keys [params]}]
  (let [fx (spline-fx)
        spx (spline-fx)
        cspx (cilia-spline-fx)
        theta-x (spline-fx)
        cilia-spline
        (dr/weighted {cilia-line 1.0
                      (partial cilia-line-plot (:fn cspx)) 2.5})]
    (reset! defo {:cspx (:params cspx)})
    (mapcat (fn [{:keys [ry amp c-amp phase]}]
              (let [screen (partial screen-space ry amp)
                    spline-pts (base-spline screen fx phase)
                    cilia (cilias screen cilia-spline fx spx c-amp theta-x phase)]
                [(csvg/path (csvg/segmented-path spline-pts))
                 (csvg/group {:stroke-width 0.75}
                   cilia)]))
            params)))

(defn scene [{:keys [scene-id] :as args}]
  (csvg/svg-timed
    {:id scene-id
     :width width
     :height height
     :stroke "black"
     :fill "none"
     :stroke-width 1.0}
    (shapes args)))

(defn explanation [{:keys [params]}]
  (ctrl/details "Debug"
                [:div (debug/pre-edn
                       (merge {:params params} @defo)
                       {:width 120})]))

(defn page [sketch-args]
  (let [params (parameters)]
    (usvg/page (assoc sketch-args
                      :params params
                      :explanation explanation)
               scene)))

(sketch/definition cilia-phase
  {:created-at "2024-10-24"
   :tags #{}
   :type :svg}
  (ctrl/mount (page sketch-args)))
