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
  (let [r (dr/weighted {1.0 1.0 0.5 1.0 2.0 1.0})
        dr (dr/weighted {1.0 1.0 0.5 1.0 2.0 1.0})
        amp (dr/weighted {0.1 1.0 0.2 1.0 0.3 1.0 0.4 1.0})
        c (dr/random)
        dc (dr/random)]
    {:params {:r r :dr dr :amp amp :c c :dc dc}
     :fn
     (fn [x] (math/sin (* eq/TAU (+ (* r x) c
                              (* amp (math/cos (* eq/TAU (+ (* dr x) dc))))))))}))

(defn cilia-spline-fx []
  (let [r (dr/weighted {1.0 1.0 0.5 1.0 2.0 1.0})
        dr (dr/weighted {1.0 1.0 0.5 1.0 2.0 1.0})
        amp (dr/weighted {0.1 1.0 0.2 1.0 0.3 1.0 0.4 1.0})
        c (dr/random)]
    {:params {:r r :dr dr :amp amp :c c}
     :fn
     (fn [dc x] (math/sin (* eq/TAU (+ (* r x) c
                                 (* amp (math/cos (* eq/TAU (+ (* dr x) dc))))))))}))

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
(defn cilias [screen-space cilia-spline line-fx length-fx cilia-amp rot-fx phase]
  (for [x (range -0.05 1.05 0.004)]
    (let [pt (screen-space line-fx x phase)
          pt' (screen-space line-fx (+ x 0.0001) phase)
          rotation (* 0.125 math/PI (rot-fx x))
          angle (+ (g/heading (tm/- pt' pt)) (* eq/TAU 0.25) rotation)
          len (* height (+ cilia-amp (* 0.75 cilia-amp (length-fx x))))]
      (cilia-spline (+ x phase) pt angle len))))

(defn line-parameters []
  (let [n (dr/weighted {(dr/random-int 3 8) 3.0
                        (dr/random-int 2 14) 1.5
                        (dr/random-int 2 20) 1.0})
        s (dr/random 1.0)
        amp (tm/clamp (dr/gaussian 0.35 0.05) 0.075 0.6)]
    (into []
          (for [y (cs/midsection (tm/norm-range n))]
            {:ry y
             :amp
             (* (/ 1.0 (inc n))
                (+ 0.025 (eq/gaussian s 0.5 -0.4 y)))
             :cilia-amp
             (* (/ 1.0 (inc n))
                (+ 0.025 (eq/gaussian amp 0.5 -0.125 y)))
             :phase
             (dr/gaussian 0.0 0.0125)}))))

(defonce defo (debug/state))

(defn shapes [{:keys [line-params]}]
  (let [line-fx (spline-fx)
        length-fx (spline-fx)
        rot-fx (spline-fx)
        cilia-fx (cilia-spline-fx)
        cilia-spline
        (dr/weighted {cilia-line 1.0
                      (partial cilia-line-plot (:fn cilia-fx)) 2.5})]
    (reset! defo {:cilia (:params cilia-fx)
                  :path (:params line-fx)
                  :length (:params length-fx)
                  :rotation (:params rot-fx)})
    (mapcat (fn [{:keys [ry amp cilia-amp phase]}]
              (let [screen (partial screen-space ry amp)
                    spline-pts (base-spline screen (:fn line-fx) phase)
                    cilia (cilias screen cilia-spline (:fn line-fx) (:fn length-fx)
                                  cilia-amp (:fn rot-fx) phase)]
                [(csvg/path (csvg/segmented-path spline-pts))
                 (csvg/group {:stroke-width 0.75}
                   cilia)]))
            line-params)))

(defn scene [{:keys [scene-id] :as args}]
  (csvg/svg-timed
    {:id scene-id
     :width width
     :height height
     :stroke "black"
     :fill "none"
     :stroke-width 1.0}
    (shapes args)))

(defn explanation [{:keys [line-params]}]
  (debug/pre-edn
   (merge {:line-params line-params} @defo)
   {:width 120}))

(defn page [sketch-args]
  (let [line-params (line-parameters)]
    (usvg/page (assoc sketch-args
                      :line-params line-params
                      :explanation-div [:div.evencols]
                      :explanation explanation)
               scene)))

(sketch/definition cilia-phase
  {:created-at "2024-10-24"
   :tags #{}
   :type :svg}
  (ctrl/mount (page sketch-args)))
